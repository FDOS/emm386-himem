/*
	emm386c.c
	
	Copyright (c) by tom ehlert 2001-2005 - all rights reserved

	Licensed under the Artistic License version
	
	please see LICENSE.TXT for details

	modified for >64M and VCPI support, Michael Devore

	Documented EMS 4.0 only supports up to 32M (0x800 pages), but some EMS
	drivers allow more than 32M.  The EMS 4.0 spec could be extended to 1G-32K
	(map value of 0xFFFF means unmap) without breaking the spec API,
    but unfortunately breaking unsophisticated programs which get confused.
	I have arbitrarily decided to allow up to 32M of EMS allocations,
	leaving the high bit of page allocation alone since specific drivers
	may make use of high bit values other than 0xFFFF for their own purposes,
	or may only check high bit for unmap -- as FreeDOS EMM386 does.

	Michael Devore's changes are not copyrighted and are released
	to the public domain.
	This does not affect copyright on the rest of the code.

	this code is based on ct0890s (copyright 1990 by harry albrecht, c't)
	
	this was taken, hacked, patched, tweaked, to make a real EMM386

	for current operational status see status.txt

*/

#define PROGRAM "EMM386"

#include "useful.h"

#define VDS

/******************** globals in EMM386.ASM **************************************/
#define DMA_BUFFER_SIZE 64
#define MAXK_EMS_ALLOWED (512L*1024L-32L)
#define	UMB_MAX_BLOCKS	8
/* #define	MAX_VCPI_RESERVE	(12U * 1024U)	*/

extern ushort far MAXPAGES;
extern ushort far PAGESAVAIL;
extern ushort far FRAME;
extern uchar far NoVCPI;
extern uchar far FlagNOEMS;
extern uchar far NoPageFrame;
extern uchar far SB;
extern uchar far NoAltBoot;
extern uchar far NoDisableA20;
extern uchar far IsXMSTableNotFixedEMS;
extern ulong far PtrXMShandleTable;
extern ushort far XMS_CONTROL_HANDLE;  
extern ulong  far MONITOR_ADDR;
extern ulong  far EMM_MEMORY_END;
extern ulong  far TOTAL_MEMORY;
extern ulong  far FIRSTPAGE;
extern ulong  far POTENTIAL_EMSVCPI_MEMORY;  
extern ulong  far MAXMEM16K;  
extern char   far IllegalOpcodeHandler[];
extern char   far startup_verbose;
extern char   far INIT_DONE;
extern char   far MEMCHECK;
extern char   far NOCHECK;
extern char   far ENDALLOC;
extern char   far EMM_Driver_Name[];
extern struct xmschain far UMBhandler; 

extern char far FlagKILLAbove64M;	/* default emm386: ON, emm38664: off
                                       if set, kill XMS memory beyond 64M  */


extern struct {
	ushort segment;
	ushort size;
/* VDS info removed
	ulong  linearaddress_bottom;
	ulong  linearaddress_top; 
	ulong  physical_offset;
*/
	} far UMBsegments[UMB_MAX_BLOCKS];	/* UMB block 'array' :-) */

struct streg32
{
	unsigned int eax_low;
	unsigned int eax_high;
	unsigned int ebx_low;
	unsigned int ebx_high;
	unsigned int ecx_low;
	unsigned int ecx_high;
	unsigned int edx_low;
	unsigned int edx_high;
};

extern ushort IS386(void);
extern ushort ISPROTECTEDMODE(void);

int VMwareDetect(void);


/********************** local globals   ***************************************/

/* int FlagNOEMS = FALSE;					/ * should be set from commandline */
ushort FlagFRAMEwanted = 0xe000;		/* should be set from commandline */
/* ushort FlagEMSwanted   = 8*1024;		/ * wanted EMM memory         */
unsigned long FlagEMSwanted = 0L;	/* with pool-sharing, no longer a default EMS size */



char VDS_enabled = 1;					/* only if explicitly enabled */
char MemoryRequest = 0;					/* only set if EMM= memory request */


ulong  XmsLinearAdress;					/* address for our allocated memory */
ulong  XmsAllocatedBytes;				/* memory reserved for EMM          */
ulong  XmsHighestMemoryByte;			/* simply end of ALL memory         */


/*****************************************************************************/

uchar SystemMemory[256];	/* 256*4K pagesize = 1MB */
char ExcludeTest = 0;
char IncludeTest = 0;

void KillAbove64M(void);



/* set memory type , but honour "EXCLUDE=" and "INCLUDE=" types */
void SetMemoryType(ushort addr, uchar type)
{
	uchar *mem = &SystemMemory[addr >>8];
	
	if (*mem == 'I' && type != 'X')
		;
	else
		*mem = type;
}	

/*
	'R' = RAM
	'E' = EPROM
    'G' = GRAPHICS
	'V' = VMWARE allocated, but possibly re-usable via I= (0e800-0ebffh)
	
	'U' = possible UMB space, because nothing else found
	'P' = PAGEFRAME
	
	'I' = INCLUDE = forced from commandline
	'X' = EXCLUDE = forbidden from commandline
	
	
*/	

/* 
	search memory for ROMS, adapters, graphics,...
	
	builds SystemMemory map
*/	

void ScanSystemMemory(void)
{
	uint mem,i;
	uchar far * pmem;
	uchar reuse;

	
	for (mem = 0; mem < 0xa000; mem++) /* system memory - reserved */
		SetMemoryType(mem,'R');
		
	for (mem = 0xf000; ; mem++)			/* system EPROM F000..FFFF */
		{
		SetMemoryType(mem,'E');
		if (mem == 0xffff) break;
		}


/* feel free to optimize graphics memory */

	for (mem = 0xa000; mem <= 0xaFFF; mem++) 	/* VGA graphics */
		SetMemoryType(mem,'G');

	for (mem = 0xB000; mem <= 0xB7FF; mem++) 	/* MONO TEXT+graphic */
		SetMemoryType(mem,'G');

	for (mem = 0xb800; mem <= 0xbFFF; mem++) 	/* VGA TEXT */
		SetMemoryType(mem,'G');

	if (VMwareDetect())
	{
		/* exclude E800-EFFF for VMware, if present */
		/* E800-EBFF range may be recoverable via I=, so
		use 'V' instead of absolute 'X' */
		for (mem = 0xe800; mem <= 0xebff; mem++)
		{
			SetMemoryType(mem,'V');
		}
		for (mem = 0xec00; mem <= 0xefff; mem++)
		{
			SetMemoryType(mem,'X');
		}
	}

	for (mem = 0xf000; ; mem++)			/* system EPROM F000..FFFF */
		{
		SetMemoryType(mem,'S');
		if (mem == 0xffff) break;
		}

	/* scan for ROMS */

	for (mem = 0xc000; mem < 0xf000;)
		{
		uint romsize;
		pmem = MK_FP(mem,0);
		
		if ( pmem[0] != 0x55u || pmem[1] != 0xaau)
			{
								/* no adapter EPROM */
			mem += 2048/16;		/* advance by 2K    */   
			continue;
			}            

		/* ensure valid EPROM signature allocates minimum 2K for 0 size */
		/* keep romsize aligned to 2K boundary, rounding up */
		romsize = (uint)(((uint)pmem[2] + !pmem[2] + 3) / 4) * 2;
		
		if (startup_verbose)	
			printf("EPROM at %p, size %u KB\n", pmem, romsize);
								/* romsize given in 512 byte*/

		for ( i = 0; i < romsize; i+=2)
			{
				if (!IncludeTest || (mem & 0xff))
				{
					SetMemoryType(mem,'E');
				}
				else
				{
					pmem = MK_FP(mem, 0);
					reuse = 1;
					for (i = 0; i < 4096; i++)
					{
						if (*(pmem+i) != 0 && *(pmem+i) != 0xff)
						{
							SetMemoryType(mem,'E');
							reuse = 0;
							break;
						}
					}
					/* this ROM 4K block appears reusable based on 0 or FF only values */
					if (reuse)
					{
						SetMemoryType(mem,'I');
						mem += 2048/16;	/* advanced by 2K to make 4K total */
					}
				}
				mem += 2048/16;		/* advance by 2K    */
			}
		}

	/* if ExcludeTest is set, we need to scan all 'U' memory and ensure
	   it is all 0 or FF values */
	if (ExcludeTest)
	{
		for (mem = 0xa0; mem < 0xf0; mem++)
		{
			if (SystemMemory[mem] == 'U')
			{
				/* this would be an upper memory block by default */
				/* don't check final byte as this can be garbage */
				pmem = MK_FP(mem << 8, 0);
				for (i = 0; i < 4095; i++)
				{
					if (*(pmem+i) != 0 && *(pmem+i) != 0xff)
					{
						/* found a nonzero, non-FF value in memory block */
						/* mark the whole block as excluded */
						SystemMemory[mem] = 'X';
						break;
					}
				}
			}
		}
	}

	/* for (i = 0xa0; i < 0xf8; i++)
			printf("%x00 : %c\n",i,SystemMemory[i]); */
}

/* 
	find a contiguous area of 64 KB 
	if none is found, EMS should be disabled
	should handle commandline option like "PAGEFRAME=0xd000"
*/	
ushort LocatePageFrame(void)
{   
	int base,i;
	ushort frame = 0;
	uchar Searching = 0;

	if (FlagNOEMS || NoPageFrame)
		{
		/* FRAME = 0; */ 
		return 0;
		}

	if (FlagFRAMEwanted)
		{
		base = FlagFRAMEwanted >> 8;

		for (i = 0; i < 16; i++)
			{
			if (SystemMemory[base+i] != 'U')
				break;
			}
		if (i == 16)
			{
			frame = base;
			goto frameset;
			}							
		printf("selected page frame %04x not available, searching automatically\n", FlagFRAMEwanted);
		Searching = 1;
		}

	for (base = 0xa0; base <= 0xf0; base++)
		{		
		for (i = 0; i < 16; i++)
			{
			if (SystemMemory[base+i] != 'U')
				break;
			}
		if (i == 16)
			{
			frame = base;
			}							
		}	 

	if (frame == 0)
		{
/*		printf("no suitable page frame found, which we can't handle\n");	*/
		printf("no suitable page frame found.  EMS functions limited.\n");
		NoPageFrame = TRUE;
		return 0;
		}


frameset:
	if (startup_verbose || Searching)
		printf("using PAGEFRAME %02x00:0000\n", frame);

	fmemset(SystemMemory+frame,'P',16);
		
	return frame << 8;
}

/* old:check if there is 16 KB contiguos memory here to be used as UMB */ 
/* new:check if there is 4K KB contiguous memory here to be used as UMB */ 

int isUMBMemory(ushort base)
{
/*	int i;
	
	for (i = 0; i < 4; base++,i++)	*/
		if (SystemMemory[base] == 'U' ||
		    SystemMemory[base] == 'I' ||
			( (FlagNOEMS || NoPageFrame) && SystemMemory[base] == 'P') )
			;
		else
			return FALSE;			
	
	return TRUE;
}


/*
	return number of pages, we need to do UMB mapping ,
	return value is in 4K pages
	does not count mapping at FF00
*/
UMBpageswanted(void)
{
	int i,wanted = 0;
/*	for (wanted = 0, i = 0xa0; i < 0xf0; i+=4)	*/
	for (wanted = 0, i = 0xa0; i < 0xf8; i++)
		if (isUMBMemory(i))
			{
			wanted++;
			}

	return wanted;
}	

/*
	make sure we have an XMS handler + enable A20
	
	return 1 for SUCCESS
*/

void (far *XMSdriverAdress)(void);

ushort xmsax,xmsbx,xmscx,xmsdx;

/* removed superfluous register loads */
int xmscall(uint function)
{
	asm mov dx,xmsdx
/*	asm mov cx,xmscx	*/
	asm mov bx,xmsbx
/*	asm mov ax,xmsax	*/
	asm mov ah,function

	XMSdriverAdress();

	asm mov xmsdx,dx
/*	asm mov xmscx,cx	*/
	asm mov xmsbx,bx
	asm mov xmsax,ax

	return xmsax;
}      

struct streg32 reg32;

/* various machinations required since TCC doesn't support 32-bit inline asm */
int xmscall32(uint function)
{
	asm db 0x66
	asm mov dx,reg32.edx_low
	asm db 0x66
	asm mov bx,reg32.ebx_low
	asm mov ah,function

	XMSdriverAdress();

	asm	db 0x66
	asm mov	reg32.edx_low,dx
	asm	db 0x66
	asm mov	reg32.ecx_low,cx
	asm	db 0x66
	asm mov	reg32.ebx_low,bx
	asm	db 0x66
	asm mov	reg32.eax_low,ax

	return reg32.eax_low;
}

int VMwareDetect()
{
	asm	db 0x66			/* mov eax,564d5856h */
	asm	mov ax,0x5868
	asm	dw 0x564d
	asm	db 0x66			/* mov ecx,0ah */
	asm mov cx,0x0a
	asm	dw 0x0000
	asm db 0x66			/* mov ebx,ecx */
	asm	mov bx,cx
	asm	db 0x66			/* mov edx,5658h */
	asm	mov dx,0x5658
	asm	dw 0x0000
	asm	db 0x66			/* in eax,edx */
	asm	in ax,dx
	asm	db 0x66			/* cmp ebx,564d5868h */
	asm cmp bx,0x5868
	asm	dw 0x564d
	asm	jne failed
	return 1;

failed:
	return 0;
}

ulong C_PtrXMShandleTable = 0;
uchar C_IsXMSTableNotFixedEMS = 1;

int XMSinit(void)
{   
	{
   asm     mov ax, 4300h;
   asm     int 2fh;                 /*  XMS installation check */

   asm     cmp al, 80h;
   asm     jne not_detected;

   asm     mov ax, 4310h;           /*  XMS get driver address */
   asm     int 2fh;
        
   asm     mov word ptr XMSdriverAdress, bx;
   asm     mov word ptr XMSdriverAdress+2, es;

   asm     mov ax, 4309h;           /*  XMS get xms handle table */
   asm     int 2fh;

   asm     cmp al,43h;
   asm     jne no_table;

   asm     mov word ptr C_PtrXMShandleTable, bx;
   asm     mov word ptr C_PtrXMShandleTable+2, es;

   asm     jmp is_table

no_table:
	asm     mov C_IsXMSTableNotFixedEMS,0
        
	} 

is_table:

	if (XMSdriverAdress == NULL)
		return 0;

	if (!xmscall(3) )				/* enable A20 */
		return 0;

	{ 						
	int i;
	for (i = 0; i < 0xf0; i++)
		{
		if (!xmscall(3) )				/* enable A20 */
			return 0;
		if (!xmscall(5) )				/* enable A20 */
			return 0;
		}			
	}


	return 1;	


not_detected:
cant_enable:
	return 0;
	
}

/*
	memory limit remarks are no longer true

	allocate memory from XMS
	find highest memory address
	determine monitor load address (must currently be <= 15MB

	this code uses only old functions (<64MB);
	EMM386 seems not yet to be prepared to handle >64MB
	

*/

XMSallocAndInitMem(unsigned long kbneeded, unsigned long kbwanted)
{   
	unsigned long xmslargest;
	unsigned long xmstotal;
	unsigned long preallocate;
	unsigned long ulcalc;
	ushort xmshandle, temphandle;
	int xmsspec3 = 0;
	int badstatus;
/*	unsigned long reserve = 0;	*/

	if (xmscall(0) && xmsax >= 0x300)	/* see if version 3.0 XMS spec */
		{
			xmsspec3 = 1;
		}

	xmsbx = 0;
	if (xmsspec3)
	{
		xmscall32(0x88);  /* query free extended memory */
		badstatus = reg32.ebx_low & 0xff;
		xmslargest = ((unsigned long)reg32.eax_high << 16) + reg32.eax_low;
		xmstotal   = ((unsigned long)reg32.edx_high << 16) + reg32.edx_low;
	}
	else
	{
		xmscall(8);  /* query free extended memory */
	    badstatus = xmsbx & 0xff;
		xmslargest = xmsax;
		xmstotal   = xmsdx;
	}
	if (badstatus)	/* failed xms call */
		{
		printf("can't get XMS information\n");
		return 0;
		}                     

	if (startup_verbose)	
		printf("XMS largest block 0x%lx(%lu), XMS total mem 0x%lx(%lu)\n",
					xmslargest,xmslargest,xmstotal,xmstotal);

/*  need 1/1024th the total for page tables */
	kbneeded += ((xmstotal+1023) >> 10);

/* reality check to throttle requests far beyond available XMS, later actual
	adjustments	are small and need not be compensated for here */
	if (kbwanted + kbneeded > xmstotal && kbneeded < xmstotal)
	{
		kbwanted = xmstotal - kbneeded;
	}

	if (MemoryRequest)
	{
		/* fixed EMS/VCPI allocation */
		/* allow for UMB allocations and rounding up to 16K boundary */
		POTENTIAL_EMSVCPI_MEMORY =
			(kbwanted + 15 + (4 * (UMBpageswanted() + 1))) * 1024UL;
	}
	else
	{
		/* shared EMS/VCPI allocation */
		POTENTIAL_EMSVCPI_MEMORY = xmstotal * 1024UL;
	}

/* if VCPI, need ((XMS total / 1.5M) + 128) * 64 bytes for pool allocation
	table entries, 1.5M is pool allocation maximum memory control,
	128 is max number XMS handles, 64 is pool block size,
	NOVCPI needs only 64 bytes for single pool allocation block
	Remember that even with NOEMS, allocations are allowed,
	so storage must be provided
*/
	if (!NoVCPI)
	{
		ulcalc = POTENTIAL_EMSVCPI_MEMORY / (1536 * 1024L);	/* bytes */
		ulcalc = (ulcalc + 128) * 64;
		ulcalc = (ulcalc + 1023) / 1024UL;	/* convert bytes back to K */
		kbneeded += ulcalc;

		ulcalc = (POTENTIAL_EMSVCPI_MEMORY + 1023) / 1024UL;	/* bytes to K */
		if (ulcalc > MAXK_EMS_ALLOWED)
		{
			ulcalc = MAXK_EMS_ALLOWED;
		}

		/* dword / 16K for EMS page pointer storage */
		ulcalc = 4 * (ulcalc + 1023) / 16384L;
		kbneeded += ulcalc;

		/* EMS page table tracker, 1 byte/16K */
		ulcalc = (ulcalc + 3) / 4L;	/* 16K EMS pages (each K-byte controls 16M) */
		kbneeded += ulcalc;

		/* add 2K for EMS names, even if NOEMS */
		kbneeded +=2;
		/* and 2K for status table of EMS handles */
		kbneeded +=2;
	}
	else
	{
		kbneeded++;	/* small NOVCPI additions */
	}

/*	if (MEMCHECK)	*/
	if (!NOCHECK)
	{
		kbneeded += 16;	/* add room for four scratch page tables */
	}

	kbwanted = (kbwanted + 15) & 0xfffffff0l;

						/* allocate memory from XMS */

	xmshandle = 0;
#if 0
	/* first try to allocate from end portion of largest block */
	if (ENDALLOC && (kbneeded + kbwanted < xmslargest))
	{
		preallocate = xmslargest - (kbneeded + kbwanted);
		if (!xmsspec3)
		{
			xmsdx = preallocate;
			if (xmscall(9))
			{
				/* preallocation successful */
				temphandle = xmsdx;
				xmsdx = kbneeded + kbwanted;
				if (xmscall(9))
				{
					/* desired allocation successful */
					xmshandle = xmsdx;
				}
				xmsdx = temphandle;
				xmscall(0x0a);	/* release preallocation */
			}
		}
		else
		{
			reg32.edx_low = (unsigned int)(preallocate & 0xffff);
			reg32.edx_high = (unsigned int)(preallocate >> 16);
			if (xmscall32(0x89))
			{
				/* preallocation successful */
				temphandle = reg32.edx_low;
				reg32.edx_low = (unsigned int)((kbneeded + kbwanted) & 0xffff);
				reg32.edx_high = (unsigned int)((kbneeded + kbwanted) >> 16);
				if (xmscall32(0x89))
				{
					/* desired allocation successful */
					xmshandle = reg32.edx_low;
				}
				xmsdx = temphandle;
				xmscall(0x0a);	/* release preallocation */
			}
		}
	}
#endif

	/* leave a little extended memory, if possible, for programs that want some XMS */
	if ((xmslargest > kbneeded + 384UL) && (xmslargest < kbneeded + kbwanted + 384UL))
	{
		kbwanted = xmslargest - kbneeded - 384UL;
	}

	for ( ; !xmshandle; kbwanted /= 2)
		{
		if (!xmsspec3)
		{
	        xmsdx = kbneeded + kbwanted;
	        if (xmscall(9))
	        	{
	        	xmshandle = xmsdx;
	        	break;
	        	}
	        /* try largest free block based allocation */
	        if ((xmslargest > kbneeded + kbwanted / 2) &&
	            (xmslargest < kbneeded + kbwanted))
	        	{
    	        xmsdx = xmslargest;
        	    if (xmscall(9))
            	    {
                	    kbwanted = xmslargest - kbneeded;
            	      	xmshandle = xmsdx;
            	       	break;
            	    }
	        	}
		}
		else
		{
			reg32.edx_low = (unsigned int)((kbneeded + kbwanted) & 0xffff);
			reg32.edx_high = (unsigned int)((kbneeded + kbwanted) >> 16);
			if (xmscall32(0x89))
			{
				xmshandle = reg32.edx_low;
				break;
    		}
	        /* try largest free block based allocation */
   	        if ((xmslargest <= kbneeded + kbwanted / 2) ||
   	            (xmslargest >= kbneeded + kbwanted))
   	        {
       	        /* outside range, try next loop */
       	        break;
   	        }
			reg32.edx_low = (unsigned int)((xmslargest) & 0xffff);
			reg32.edx_high = (unsigned int)((xmslargest) >> 16);
       	    if (xmscall32(0x89))
           	{
           	    kbwanted = xmslargest - kbneeded;
   				xmshandle = reg32.edx_low;
           	    break;
            }
		}
		if (kbwanted == 0)
			{
			printf("can't allocate enough XMS memory(%uKB)\n", kbneeded);
			return 0;
			}		
		}

	XMS_CONTROL_HANDLE = xmshandle;

	if (startup_verbose)	
		printf("  allocated %lukb from XMS\n", kbneeded + kbwanted);

						/* lock handle to make a linear adress */	
	
	xmsdx = xmshandle;
	if (!xmscall(0x0c))
		{
		printf("can't lock XMS memory ???\n");
		return 0;
		}		

	XmsAllocatedBytes =  ((ulong)(kbwanted+kbneeded) * 1024);
	
	XmsLinearAdress	     = 	((ulong)xmsdx << 16) | xmsbx;

	XmsHighestMemoryByte =  (ulong)xmstotal * 1024L +
							 0x110000l;					/* DOS + HMA */




/*	printf("xms locked memory at %lx, top of mem %lx(%luMB) alloc bytes %lx(%luKB)\n", 
				XmsLinearAdress,
				XmsHighestMemoryByte,XmsHighestMemoryByte/(1024*1024l),
				XmsAllocatedBytes,XmsAllocatedBytes/(1024));
*/

	return 1;	
	
}   


/*
	we implement a dull (and thus small) UMB handler 
	sufficient for DOS=HIGH, but nothing else :-))

	we will manage 4 different UMB segment/size
*/

struct xmschain {
	uchar opcode;
	struct xmschain far *next;
	};
	
struct verify_alignment { char x[ sizeof(struct xmschain) == 5 ? 1 : -1]; };	


extern char far * far UMBOldhandler;
extern struct xmschain far UMBhandler; 

extern ulong far int4b_oldhandler;
extern int  far  int4b_handler; 

void InstallUMBhandler(void)
{

							/*  allocate memory block -
								should return NOT_IMPLEMENTED */
	struct xmschain far *pchain;								
	ushort mem,size,index;



    xmsdx = 0xffff;			/* this should fail with error 0x80 */
    xmscall(0x10);
    
    if ((xmsbx & 0xff) != 0x80)
    	{
    	printf("UMB handler already installed, not installing another one\n");
    	return;
    	}

	pchain = (struct xmschain far *) XMSdriverAdress;
	
	for (;;)
		{
		switch(pchain->opcode)
			{
			case  0xeb: 		/* jmp short, end reached */
				goto foundend;

			case  0xea:			/* jmp far xxxx:yyyy */
				pchain = pchain->next;
				break;
				
			default:
				printf("illegal opcode %02x in XMSchain detected - breal\n",pchain->opcode);								
				return;
			}
		}
    

foundend:


								/* now prepare UMB segments to be used later */
	index = 0;
/*	for (mem = 0xa0; mem < 0xf0; )	*/
	for (mem = 0xa0; mem < 0xf8; )	/* allow umbs in f000-f7ff */
		if (!isUMBMemory(mem))
/*			mem += 4;	*/
			mem++;	/* allow 4K UMB's	*/
		else			
			{
/*			for (size = 4; ; size+=4)	*/
			for (size = 1; ; size++)	/* 4K UMB's */
				if (!isUMBMemory(mem+size))
					break;

			UMBsegments[index].segment = mem  << 8;
			UMBsegments[index].size    = size << 8;			

#if 0
	/* EMM386 no longer disallows 0xa000 upper memory block returns */
                                           / * this could cause weird bugs 
                                              if upper memory a000 would ever
                                              be merged with 9fff:0        */
			if (UMBsegments[index].segment == 0xa000)
				{			
				UMBsegments[index].segment++;
				UMBsegments[index].size   --;			
				}
#endif


			if (startup_verbose) 
				printf(" UMB block %d at %x:0000, size = 0x%x paragraphs (%uKb)\n", 
					index,
					UMBsegments[index].segment,
					UMBsegments[index].size,
					UMBsegments[index].size / (1024/16)
					);
			index++;
			if (index >= UMB_MAX_BLOCKS)
				break;

			mem += size;				
			}

	if (UMBsegments[0].segment == 0)
		{
		printf("no suitable UMB memory block found\n");
		return;                                        
		}


								/* now patch us on the end of chain */
	UMBOldhandler  = (char far *)pchain + 5;

	pchain->opcode = 0xea;		/* jmp far	*/
	pchain->next   = &UMBhandler; 

	
}




/*
void Pause()
{
	printf("Any key please");
	asm mov ax,0;
	asm int 0x16;
	
	printf("\n");
}	
*/
/* called on startup.
	handle commandline "/IB800-BFFF /XE000-EFFF" ...
	search for EPROMS+adapters (network cards)
	determine frame address
	...
	mode = 0 if called as driver
	mode = 1 if called as EXE
	
	return: 0         - everything fine
	        errorcode - exit code/abort driver
*/ 

int TheRealMain(int mode, char far *commandline)
{
	char far *found;
	uchar SaveVar;
	

	printf( PROGRAM " 2.26 [" __DATE__ "]"
	       " (c) tom ehlert 2001-2006 c't/H. Albrecht 1990\n");

	fmemset(SystemMemory,'U',sizeof(SystemMemory));


	/******* commandline handling **********/

	if (FindCommand(commandline, "/VERBOSE", &found) ||
	    FindCommand(commandline, "/V",       &found) )
		{ 
		startup_verbose = 1;
		}

	if (FindCommand(commandline, "EMM=", &found) ||
		FindCommand(commandline, "MIN=", &found))
		{ 
		FlagEMSwanted = GetValue(found,10,TRUE);
		if (FlagEMSwanted > MAXK_EMS_ALLOWED)
		{
			FlagEMSwanted = MAXK_EMS_ALLOWED;
		}
		printf("wanted EMS memory %lu\n",FlagEMSwanted);
		MemoryRequest = 1;
		C_IsXMSTableNotFixedEMS = 0;
		}      


	if (FindCommand(commandline, "NOEMS", &found) )
		{
		if (startup_verbose)	
			printf("NOEMS: disable EMS handling (mostly :-)\n");
		FlagNOEMS = TRUE;
		if (!MemoryRequest)
		{
			/* only turn off EMS default amount, not direct request */
			FlagEMSwanted = 0;
		}
		}

	if (FindCommand(commandline, "NOVCPI", &found) )
		{
		if (startup_verbose)	
			printf("NOVCPI: disable VCPI support, requires NOEMS\n");
		if (FlagNOEMS)
		{
			NoVCPI = TRUE;
			C_IsXMSTableNotFixedEMS = 0;
		}
		}

	if (FindCommand(commandline, "NOVDS", &found) )
		{
		if (startup_verbose)	
			printf("VDS disabled\n");
		VDS_enabled = 0;
		}

	if (FindCommand(commandline, "VDS", &found) )
		{
		if (startup_verbose)	
			printf("VDS enabled\n");
		VDS_enabled = 1;
		}

	if (FindCommand(commandline, "FRAME=", &found) ||
	    FindCommand(commandline, "/P",     &found) )
		{ 
		/* avoid bringing in string functions on NONE compare */
		if (toupper(found[0]) == 'N' && toupper(found[1]) == 'O' &&
			toupper(found[2]) == 'N' && toupper (found[3]) == 'E')
		{
			NoPageFrame = TRUE;
			FindCommand(commandline, "NONE", &found);	/* advance past NONE */
		}
		else
		{
			FlagFRAMEwanted = GetValue(found,16,FALSE);
			printf("selected FRAMEwanted=0x%x\n",FRAME);
		}
		}

	if (FindCommand(commandline, "X=TEST", &found) )
		{ 
		ExcludeTest = 1;
		}

	if (FindCommand(commandline, "I=TEST", &found) )
		{ 
		IncludeTest = 1;
		}

	if (FindCommand(commandline, "SB", &found) )
		{ 
		SB = 1;
		}

	if (FindCommand(commandline, "MEMCHECK", &found) )
		{ 
		MEMCHECK = 1;
		}

	if (FindCommand(commandline, "NOCHECK", &found) )
		{ 
		NOCHECK = 1;
		}

	if (FindCommand(commandline, "MAX=", &found) )
		{ 
		MAXMEM16K = GetValue(found,10,TRUE);
		MAXMEM16K /= 16UL;
		}

/*
	if (FindCommand(commandline, "ENDALLOC", &found) )
		{ 
		ENDALLOC = 1;
		}
*/

	if (FindCommand(commandline, "NOALTBOOT", &found) )
		{ 
		NoAltBoot = 1;
		}

	if (FindCommand(commandline, "NODISABLEA20", &found) )
		{ 
		NoDisableA20 = 1;
		}

	if (FindCommand(commandline, "ALTBOOT", &found) )
		{ 
		NoAltBoot = 0;
		}

	if (FindCommand(commandline, "NOHI", &found) )
		{ 
		/* NOHI is a no-op, but helps MS EMM386 switch compatibility */
		}

	if (FindCommand(commandline, "NOMOVEXBDA", &found) )
		{ 
		/* NOMOVEXBDA is a no-op, but helps MS EMM386 switch compatibility */
		}

	if (FindCommand(commandline, "RAM", &found) ||
		FindCommand(commandline, "/?", &found))
		{
		/* ignore bare RAM option or /? */
		}
                  
		                    /* temporary for >64M support */
		
	if (FindCommand(commandline, "/DONTKILLABOVE64M", &found) )
		{ 
		FlagKILLAbove64M = 0;
		}
	if (FindCommand(commandline, "/DOKILLABOVE64M", &found) )
		{ 
		FlagKILLAbove64M = 1;
		}

					/* "I=a000-afff"  "X=d000-dfff" */
		
	for (;;)
		{
		ushort rangestart,rangestop;
		char memtype;
		
		memtype = 'I';
		
		if (FindCommand(commandline, "I=", &found) )
			;
		else {
			memtype = 'X';
			if (FindCommand(commandline, "X=", &found) )
				;
			else
				break;
			}

		rangestart =  GetValue(found,16,FALSE);

		if (*found == '-')		
			{
			fmemcpy( found, found+1, 
								fstrlen(found+1) +1); 

		
			rangestop  =  GetValue(found,16,FALSE);
		
			printf("%c=%x..%x\n",memtype, rangestart,rangestop);
			
			if (rangestart && rangestop && rangestart<=rangestop && rangestop <= 0xffff)
				for ( ; rangestart < rangestop; rangestart++)
					SetMemoryType(rangestart,memtype);
				
			}
		
		}

	/******* commandline handling done, are there remainders **********/

	commandline = skipWhite(commandline);

	if (*commandline)
		printf("ignored commandline  <%Fs>\n", commandline);



	if (mode == EXECMODE_EXE)
		{
							/* called as exe from commandline 
								do :
									usage info
									status report
									show memory statistic
									...
							*/
							
		printf("Requires 80386+ CPU.\n"
				"please load " PROGRAM " as DEVICE=" PROGRAM ".EXE in config.sys\n\n");

		printf("commandline options available for driver\n"
				" ALTBOOT     - hook keyboard interrupt for reboot processing\n"
				" EMM=#####   - reserve up to #####K for only EMS/VCPI memory\n"
				" FRAME=E000  - select wanted pageframe for EMS\n"
				" I=A000-AFFF - IF YOU REALLY KNOW WHAT YOU DO (VGA graphics)\n"
				" I=B000-B7FF - IF YOU REALLY KNOW WHAT YOU DO (Hercules)\n"
				" I=TEST      - test ROM locations for UMB inclusion\n"
				" MAX=#####   - maximum available VCPI memory in K; also EMS if <32M\n"
				" MEMCHECK    - access to full 4G address space without RAM (MMIO)\n"
				" NOALTBOOT   - do not hook keyboard interrupt for reboot processing (default)\n"
				" NOCHECK     - disallow access to address space without RAM (MMIO)\n"
				" NODISABLEA20 - Do not allow EMM386 to disable A20\n"
				" NOEMS       - disable EMS handling\n"
				" NOVCPI      - disable VCPI handling, requires NOEMS\n"
				" NOVDS       - disable Virtual DMA Services\n"
				" SB          - SoundBlaster driver compatibility mode\n"
				" VDS         - enable Virtual DMA Services (default)\n"
				" /VERBOSE    - display additional details during start\n"
				" X=D000-D800 - to make memory mapped devices work\n"
				" X=TEST      - test ROM locations for UMB exclusion\n"
				);				
				
		return 1;
		}											

	/******* options set, now process **********/

	if (!IS386())	
			{ 
			printf(PROGRAM " requires at least a 80386 to run\n"); 
			return 1;
			}

	if (ISPROTECTEDMODE())
			{ 
			printf(PROGRAM ":already in protected mode, can't continue\n"); 
			return 1;
			}

	if (FlagKILLAbove64M)
			{
								/* EMM386 isn't able yet do work >64M
								   so (for now) we simply allocate
								   and forget everything beyond 64M
								*/
			KillAbove64M();								   
			}			



	SaveVar = C_IsXMSTableNotFixedEMS;
	if (!XMSinit())
			{ 
			printf(PROGRAM ":no XMS handler found, required\n"); 
			return 1;
			}

	if (SaveVar && !C_IsXMSTableNotFixedEMS)
	{
		/* no int 2fh, function 4309h support */
		printf("\nWARNING: No INT 2Fh function 4309h support in current XMS memory manager\n");
		printf("Use EMM386 EMM= option for EMS and VCPI memory, or update XMS memory manager.\n\n");
	}

	PtrXMShandleTable= C_PtrXMShandleTable;
	IsXMSTableNotFixedEMS = C_IsXMSTableNotFixedEMS;

	ScanSystemMemory();			/* build up system memory map */


	if (!FlagNOEMS && !NoPageFrame)
		{

		FRAME = LocatePageFrame();	/* find a contiguos area of 64 KB */	
	
		if (startup_verbose)	
			printf("  choosen FRAME address %x\n",FRAME);
	
	                   				/* some error checks, return != 0 --> exit */
/* allow no frame by turning on NoPageFrame flag */
		if (FRAME == 0)
			NoPageFrame = 1;
		}

							/* 
								allocate from XMS the memory we need 

								this is memory for UMB's, including FF00
								
								+ 64KB for the monitor...
								+ 64KB for DMA buffering
								+ room for other control structures made inside function
								
								+ what the user wants for EMS
							*/

	if (!XMSallocAndInitMem((UMBpageswanted()+1) * 4 + 64 + 64, FlagEMSwanted))
		{
		return 1;
		}


	MONITOR_ADDR   = XmsLinearAdress;
	EMM_MEMORY_END = XmsLinearAdress + XmsAllocatedBytes;
	TOTAL_MEMORY   = XmsHighestMemoryByte;  

	if (startup_verbose)	
		printf("  MONITOR_ADDR  %lx EMM_MEMORY_END %lx TOTAL_MEMORY  %lx\n",
			MONITOR_ADDR,EMM_MEMORY_END, TOTAL_MEMORY);

	/* InstallUMBhandler();		/* as long as we can debug it */
			
			




/*	Pause(); */

	return 0;				/* OK so far , continue */		
}



/* called just before we go resident
*/ 
void MyFunnyMain(void);
 
void far finishing_touches()
{ 
	char far *p;


	if (startup_verbose) 
	{
		emmcall(0x42);	/* kickstart dynamic count of available EMS pages */
	 	printf("  total(available) EMS %d(%d) pages = %lu(%lu) kByte\n",
			MAXPAGES, PAGESAVAIL, (ulong)MAXPAGES*16, (ulong)PAGESAVAIL*16);
	}



				/* correct int67 offset. this is a bug in fdemm */    
				/* is it really ?? */
	
	p = *(char far *far*)MK_FP(0,4*0x67);

	if (p == NULL) {printf("no int67");return;}


	if (fmemcmp(p+10, "EMMXXXXX0",3) != 0) 
		{
		if (fmemcmp(MK_FP(FP_SEG(p),10), "EMMXXXX0",8) == 0) 
			{ 
/*			printf("correcting int67 offset\n"); 
			
			*(ushort far*)MK_FP(0,4*0x67) = 0;			*/
			}
		else
			hexd("Int67 offset %p is somehow wrong\n",p,16);
		}			


	*(ulong far *)MK_FP(0,6*4) = (ulong) &IllegalOpcodeHandler;


				/* now do the fun stuff */

	MyFunnyMain(); 

/*	printf("\n--> %lx <--\n", FIRSTPAGE);	*/
}



ushort emmfunction,emmax,emmbx,emmcx,emmdx;

int emmcall(uint function)
{
	emmfunction = function;
	
	emmax = (emmax & 0xff ) | (function << 8);

/*	printf("%25s in  %04x %04x %04x %04x -", "",emmax,emmbx,emmcx,emmdx);
*/
	asm mov dx,emmdx
	asm mov cx,emmcx
	asm mov bx,emmbx
	asm mov ax,emmax

	asm int 0x67

	asm mov emmdx,dx
	asm mov emmcx,cx
	asm mov emmbx,bx
	asm mov emmax,ax

/*	printf("%1s out %04x %04x %04x %04x \n", "",emmax,emmbx,emmcx,emmdx);
*/
	return emmax >>8;
	
}      
void emmerror(char *s)
{
	printf("EMM failed: %s\n",s);
	
	printf("func %02x, out %04x %04x %04x %04x \n",emmfunction, emmax,emmbx,emmcx,emmdx);
}




/*  
	post processing:
	
	here some magic happens:
	we use (mostly) standard EMS functionality to map some memory
	at the UMB locations + do some more cleanup
*/




/* VDS code here has been removed due to changes in the way it works */
void MyFunnyMain()
{   
	int i;
	ushort pageswanted; 
	ushort emmhandle = 0;	/* allocate UMBs via system handle */
	ushort logicalpage; 
/*	#ifdef VDS	*/
#ifdef REMOVEVDS
	int    VDS_index = 0;
#endif	

	INIT_DONE	= 0;
	
	
	/* check install state */
/*	
	emmcall(0x46);
	printf("version %x\n",emmax);

	emmcall(0x41);
	printf("page frame at %x\n",emmbx);
	
	emmcall(0x42);
	printf("total pages %x(%dMB), free %x(%dMB)\n",emmdx,emmdx/(1024/16),emmbx,emmbx/(1024/16));

	emmcall(0x4b);
	printf("emm handles %x\n",emmbx);
*/
/*
	for (mem =0xa0; mem < 0xf8; mem+=4)
		printf("mem %x %c - %d\n",mem,SystemMemory[mem], isUMBMemory(mem));
*/

	pageswanted = UMBpageswanted() + 1;	/* add 1 for ROM FF00h shadowing */

	/* allocate a handle + some memory */

	if (startup_verbose)                                                 
		printf("  allocating %u 4K pages = %uKByte for UMB's\n", 
/*									pageswanted,pageswanted*16); */
									pageswanted,pageswanted*4);

	if (!pageswanted)
	{
		goto dont_install_umbhandler;
	}

/* UMB allocations done through system handle via reallocation, since
 system handle is already present with zero assigned pages */
	emmdx = emmhandle;
	emmbx = (pageswanted+3)/4;	/* round up */
/*	if (emmcall(0x43) != 0)	*/
	if (emmcall(0x51) != 0)
		{
/*		printf("  allocating %dKByte for UMB's:",pageswanted*16);	*/
		printf("  allocating %dKByte for UMB's:",pageswanted*4);
		emmerror("");
		return;
		}

/*    emmhandle = emmdx;	*/


	/*  here comes the funny part:
		during initialization phase, calling EMM_MAP_PAGE (0x44)
		with pysical page (AL) > 3 is possible.
		meaning:
		
		AL = highest 8 bits of logical adress, AL=E4 --> address E4000

		initialization phase is terminated with AL=9F
		
	*/	

								/* map pages in to the UMB area */

	if (startup_verbose)	
/*		printf(" mapping UMBs (16K each) at:");	*/
		printf(" mapping UMBs (4K each) at:");

	logicalpage = 0;
	

/*	for (i = 0xa0; i < 0xf0; i+=4)	*/
	for (i = 0xa0; i < 0xf8; i++)	/* allow 4K UMB's */
		if (isUMBMemory(i))
			{
			if (startup_verbose)	
				printf("%x00 ",i);     
		
			emmax = i;
			emmbx = logicalpage;
			emmdx = emmhandle;
		
			if (emmcall(0x44))
				{
				emmerror("mapping UMB page !!");
				goto dont_install_umbhandler;
				}   

/*	#ifdef VDS */
#ifdef REMOVEVDS
									/* stuff for VDS service */
			                                   
			if (VDS_enabled)
				{	
				ulong physical_address;		                                   
										/* this crazy call returns physical address in DX:BX	*/
										
				physical_address = ((ulong)emmdx <<16) | (ushort)(emmbx & ~7);
				/* printf("mapped linear address %02x --> %lx\n", i, physical_address); */


				if (i != 0xa0 && isUMBMemory(i-4))	/* merge with previous */
					{
					UMBsegments[VDS_index].linearaddress_top += 0x4000;
					}
				else {          
					
					UMBsegments[VDS_index].linearaddress_bottom = ((ulong)i) << 12;
					UMBsegments[VDS_index].linearaddress_top    = 
							UMBsegments[VDS_index].linearaddress_bottom + 0x4000;	
					UMBsegments[VDS_index].physical_offset      = physical_address
													- UMBsegments[VDS_index].linearaddress_bottom;	
					}

				if (!isUMBMemory(i+4) || i == 0xf0-4)
					{              
					VDS_index++;
					}
				
				}
#endif				
				
			logicalpage++;
			}
			
/*	#ifdef VDS	*/
#ifdef REMOVEVDS
	if (VDS_enabled)
		{
		if (startup_verbose)	
			for (i = 0; i < VDS_index; i++)
				{
				printf("VDS region %lx..%lx += %lx\n",
						UMBsegments[i].linearaddress_bottom,
						UMBsegments[i].linearaddress_top   ,
						UMBsegments[i].physical_offset     
						);
				}
		int4b_oldhandler = 	*(ulong far *)MK_FP(0,0x4b*4);
		*(ulong far *)MK_FP(0,0x4b*4) = (ulong)&int4b_handler;
		*(uchar far *)MK_FP(0,0x47b)  |= 0x20;
		}
#endif
	/* map block at F000-FFFF so we can trap jumps to FFFF:0 for rebooting */
	/* the 0xff physical page flags EMM386 to copy over ROM image */
	emmax = 0xff;
	emmbx = logicalpage;
	emmdx = emmhandle;
	if (emmcall(0x44))
	{
		emmerror("mapping block FF00h!");
	}

	if (startup_verbose)	
		printf("\n");

							/* finish our funny 'enhanced' EMM_MAP_HANDLE behaviour */
	emmax = 0x9f;
	emmdx = emmhandle;
		
	if (emmcall(0x44))
			{
			emmerror("finish fun part");
			return;
			}

							/* verify the fun part really finished */
	emmax = 0x9f;
	emmdx = emmhandle;
		
	if (!emmcall(0x44))
			{
			emmerror("mapping UMB memory");
			}


	InstallUMBhandler();		/* as long as we can debug it */
 
dont_install_umbhandler:
	INIT_DONE = 1;	/* flag initialization done here in case UMB code bypassed */

	if (VDS_enabled)
	{
		int4b_oldhandler = 	*(ulong far *)MK_FP(0,0x4b*4);
		*(ulong far *)MK_FP(0,0x4b*4) = (ulong)&int4b_handler;
		*(uchar far *)MK_FP(0,0x47b)  |= 0x20;
	}

	if (FlagNOEMS)
		{
		/* change EMMXXXX0 to EMMQXXX0 to flag NOEMS for driver */
		*(EMM_Driver_Name+3) = 'Q';
					/* disable EMM completely in real mode. hack. */
		if (NoVCPI)
		{
			*(ulong far *)MK_FP(0,4*0x67) = 0;
		}
		else
		{
			/* also change EMS identifer to EMMQXXX0 */
			char far * p = *(char far *far*)MK_FP(0,4*0x67);
			uint uip = FP_SEG(p);
			p = MK_FP(uip,0);
			*(p+13) = 'Q';
		}

		}

	return;


}




/**************************************************************************
	stuff needed to kill memory above 64M
**************************************************************************/

#ifdef __TURBOC__
    #pragma warn -asc           /* suppress warning 'switching to assembly */
    #define _asm asm
    void __int__(int);
#endif    



void (far *kill_xmsPtr)() = 0;

unsigned kill_XMSax,kill_XMSbx,kill_XMSdx;



int kill_XMScall( unsigned rAX, unsigned rDX)
{
    if (kill_xmsPtr == NULL)
        {
        _asm mov ax, 4300h
        _asm int 2fh                /* XMS installation check */

        _asm cmp al, 80h
        _asm jne detect_done

        _asm mov ax, 4310h               /* XMS get driver address */
        _asm int 2fh

        _asm mov word ptr [kill_xmsPtr+2],es
        _asm mov word ptr [kill_xmsPtr],bx
        
        
        if (startup_verbose) printf("XMShandler located at %x:%x\n", FP_SEG(kill_xmsPtr),FP_OFF(kill_xmsPtr));
        }
detect_done:

    if (kill_xmsPtr == NULL)
        return 0;

    _asm    mov  ax,rAX
    _asm    mov  bx,kill_XMSbx
    _asm    mov  dx,rDX
    _asm    call dword ptr [kill_xmsPtr]
    
    _asm    mov kill_XMSax,ax
    _asm    mov kill_XMSbx,bx
    _asm    mov kill_XMSdx,dx

    return kill_XMSax;    

}                

#define LENGTH(x) (sizeof(x)/sizeof(x[0]))


 
void KillAbove64M()
{
	unsigned largestfree;
	unsigned handle;
	ulong 	 physoffset,physoffsetend;
	unsigned numhandles = 0;
	unsigned i;
	struct {
	    unsigned handle;
	    } hTable[10];
	
	
	
	/* asm int 3; */
	
	
	if (!kill_XMScall(0,0))           /* get version number */
		{
		printf(PROGRAM ":no XMS handler detected\n");
		return;
		}

		
    for (numhandles = 0; numhandles < LENGTH(hTable); )
        {    
        kill_XMScall(0x0800,0);  /* query free extended memory */
        
        largestfree = kill_XMSax;


    	if (startup_verbose) printf("free memory total %uK, largest %uk\n", kill_XMSdx, largestfree);
    	
    	if (largestfree == 0)
    		break;


								/* allocate biggest chunk possible */    	
        kill_XMScall(0x0900,largestfree);  /* allocate extended memory */
        
        if (kill_XMSax == 0)
        	{
        	printf(PROGRAM ":OUTCH ??\a\n");	/* this should have succeeded */
            break;
            }       

		handle = kill_XMSdx;            

        kill_XMScall(0x0c00,handle);
        if (kill_XMSax != 1)
        	{
            printf(PROGRAM ":\acan't lock - %02x\a\n", kill_XMSbx & 0xff);
            break;
            }
            
        
		physoffset    = (((ulong)kill_XMSdx)<<16) + kill_XMSbx;
		physoffsetend = physoffset+(ulong)largestfree*1024-1;
		
        kill_XMScall(0x0d00,handle);		/* unlock */
		

		if (startup_verbose) printf(" handle %x at %lx..%lx\n",handle,physoffset, physoffsetend);


		if (physoffset >= 64l*1024*1024)
			{
					/* it's all above 64M - 'forget it' */
					
			if (startup_verbose) printf(PROGRAM ":memory at %lx..%lx 'killed'\n",physoffset, physoffsetend);
			continue;					
			}


        hTable[numhandles].handle = handle;		/* to be freed later */
        numhandles++;

		if (physoffsetend > 64l*1024*1024)
			{
					/* shrink it, so it doesn't run into 64M anymore */

			kill_XMSbx = (unsigned)((64l*1024*1024 - physoffset) /1024);

			if (startup_verbose) printf("shrinking to %uK\n",kill_XMSbx);
			
			kill_XMScall(0x0f00,handle);
			
			}
        }

	for ( i = 0; i < numhandles; i++)
		{
		if (startup_verbose) printf("freeing handle %x\n", hTable[i].handle);
		kill_XMScall(0x0a00,hTable[i].handle);    
		}

	return;
}

