/****************************************************************/
/*                                                              */
/*                          XMStest.c                           */
/*                                                              */
/*   verifikation module for (some) XMS functions               */
/*                                                              */
/*                      Copyright (c) 2001                      */
/*                      tom ehlert                              */
/*                      All Rights Reserved                     */
/*                                                              */
/* This file is part of DOS-C.                                  */
/*                                                              */
/* DOS-C is free software; you can redistribute it and/or       */
/* modify it under the terms of the GNU General Public License  */
/* as published by the Free Software Foundation; either version */
/* 2, or (at your option) any later version.                    */
/****************************************************************/

/*
    XMStest.c
    
    verifies functionality of an XMS handler
    
    compilable (at least) with TC 2.01 + TASM
    
    
    functions tested:
    
    XMSalloc()
    XMSfree() (random order)
    XMSmove(seg:offset --> HANDLE,
            HANDLE     --> seg:offset
            HANDLE     --> HANDLE
           ) 

    XMS memory is verified for correct content.
    
    XMSlock/unlock
    
    
    COMPILER supported : 
        TurboC 2.01, (TCPP 1.x ?) - TASM required 
        MS-C for 16-bit
    
*/


#include "useful.h"
/*#include <stdio.h>
  #include <dos.h>
  */

typedef unsigned long ulong;

#ifdef __TURBOC__
    #pragma warn -asc           /* suppress warning 'switching to assembly */
    #define _asm asm
    void __int__(int);
#endif    


void (far *xmsPtr)() = 0;

unsigned XMSax,XMSbx,XMSdx;


unsigned errors_occurred = 0;
#define ERROR	errors_occurred++,printf



/*  return the system timer tick count - should be identical to clocks() */
ushort ticks(void)
{
	return *(ushort far *)MK_FP(0x40,0x6c);	
}


/* srand(), rand() - replacement to be RTL free
   note: the numbers choosen below are BULLSHIT, someone should
   lookup correct values for it.
   however for our purpose this should work.
*/

ushort __RAND_CURR_VAL = 1234;

void __srand(ushort val)
	{
	__RAND_CURR_VAL = val;
	}

short __rand(void)
	{
	return (__RAND_CURR_VAL = __RAND_CURR_VAL * 17 + 65) & 0x7fff;
	}
	





int XMScall( unsigned rAX, unsigned rDX)
{
    if (xmsPtr == NULL)
        {
        _asm mov ax, 4300h
        _asm int 2fh                /* XMS installation check */

        _asm cmp al, 80h
        _asm jne detect_done

        _asm mov ax, 4310h               /* XMS get driver address */
        _asm int 2fh

        _asm mov word ptr [xmsPtr+2],es
        _asm mov word ptr [xmsPtr],bx
        
        
        }
detect_done:

    if (xmsPtr == NULL)
        return 0;

    _asm    mov  ax,rAX
    _asm    mov  bx,XMSbx
    _asm    mov  dx,rDX
    _asm    call dword ptr [xmsPtr]
    
    _asm    mov XMSax,ax
    _asm    mov XMSbx,bx
    _asm    mov XMSdx,dx

    return XMSax;    

}                

#define LENGTH(x) (sizeof(x)/sizeof(x[0]))

struct {
    unsigned handle;
    unsigned size;
    } hTable[100];


/*
    fill a 1 K buffer with (handlenum << 28) 
*/

void fillMem(long *buff,int hnum, int offsetK)
{
    long start = ((long)hnum << 28) | (offsetK << 10);
    int i;
    for (i = 0; i < 256; i++)
        buff[i] = start + i;
}    

long buff1[256];
long buff2[256];

XMSmove(unsigned dhandle, long doffset,
        unsigned shandle, long soffset,
        long length)
{
    struct {
        long length;
        short shandle;
        long soffset;
        short dhandle;
        long  doffset;
        } C;
   C.length =   length;  
   C.shandle = shandle;  
   C.soffset=  soffset; 
   C.dhandle=  dhandle; 
   C.doffset=  doffset; 

/*
	printf("copy in - [%x:%x]=len %lx s %x,%lx d %x,%lx\n",
		FP_SEG(&C), FP_OFF(&C),
	    C.length,
   		C.shandle,
   		C.soffset,
		C.dhandle,
   		C.doffset);  
*/   		
	

    _asm    mov  ax,0x0b00;
    _asm    lea si,C;
    _asm    call dword ptr [xmsPtr]
    
    _asm    mov XMSax,ax
    _asm    mov XMSbx,bx


    if (XMSax != 1)
        {
        printf("Error %02x while moving (%u,%lx) <-- (%u,%lx), len %u\n",
            (XMSbx & 0xff),
            dhandle, 
            doffset,
            shandle,
            soffset,
            length);  
        }            
    return XMSax == 1 ? 0 : (XMSbx & 0xff);
            
}


#ifndef TEST_XMS_REALLOC 

	#define test_xms_realloc()

#else

/*
	this caused trouble to HIMEM,FDXMS < 12/10/01
	as it was going to loose handles / fragment memory
	solved for HIMEM >= 12/10/01
*/



void test_xms_realloc(void)
{
	uint maxfree,freetotal;
	uint handle,i;

	printf("testing XMS_REALLOC\n");

    XMScall(0x0800,0);  /* query free extended memory */
    
    freetotal = XMSdx;
    maxfree   = XMSax;

	printf("allocating %uK\n",maxfree);    

    if (XMScall(0x0900,maxfree) != 1)  /* allocate extended memory */
    	{
    	ERROR("could not allocate - %x\n",XMSbx & 0xff);
    	return;
    	}
    
    handle = XMSdx;
    
    for (i = 8; i >= 1; i--)
    	{
    	XMSbx = maxfree/8*i;
    	
    	printf("resizing to %uK - ",XMSbx);
    	
    	if (XMScall(0x0f00,handle) != 1)  /* allocate extended memory */
    		{
    		ERROR("can't realloc %x\n",XMSbx & 0xff);
    		}
		else
			if (verbose) printf("\n");    		
		}

	XMSbx = 1;
   	printf("resizing to %uK\n",1);
    	
   	XMScall(0x0f00,handle);			  /* allocate extended memory */

    XMScall(0x0800,0);  /* query free extended memory */

	
	printf("after realloc :   total@end %u ,   total@start %u\n",
											XMSdx,freetotal);
	printf("after realloc : maxfree@end %u , maxfree@start %u\n",
											XMSax,maxfree);


end:
	
    XMScall(0x0a00,handle);    /* free handle */

    XMScall(0x0800,0);  /* query free extended memory */
    
	if (freetotal != XMSdx) 
			ERROR("after realloc :   total@end %u !=   total@start %u\n",
											XMSdx,freetotal);
	if (maxfree   != XMSax) 
			ERROR("after realloc : maxfree@end %u != maxfree@start %u\n",
											XMSax,maxfree);
	
}
#endif /* TEST_XMS_REALLOC */

XMSTESTmain(char verbose)
{
    int numhandles;
    unsigned maxfree;
    unsigned long totalallocated = 0;
    unsigned i,offset;
    unsigned freeMemAtStart,freeMemAtEnd;
    
    unsigned startClock = ticks();
    unsigned endClock;
    

    printf ("HIMEM is testing the extended memory in your system . . .\n");

	if (!XMScall(0,0))           /* get version number */
		{
		printf("no XMS handler detected\n");
		return 1;
		}
    
    if (verbose)
	{
    	printf("XMShandler located at %x:%x\n", FP_SEG(xmsPtr),FP_OFF(xmsPtr));
    	printf("version info interface %04x revision %04x exists %04x\n", XMSax,XMSbx,XMSdx);
	}
    
    XMScall(0x0800,0);  /* query free extended memory */

    freeMemAtStart = XMSax;
            
    if (verbose)
    	printf("max free memory is %u\n",freeMemAtStart);     
    
    if (!XMScall(0x0300,0))
		printf("can't enable A20 ??\n");
    


    for (numhandles = 0; numhandles < LENGTH(hTable); numhandles++)
        {    
        XMScall(0x0800,0);  /* query free extended memory */
        
        maxfree = XMSax;
        
        if (maxfree == 0)
            break;
    
    	if (maxfree > 30)        
        	maxfree = maxfree/2 + 20;

    	if (verbose)
        	printf("free %5u, largest %5u, allocating %5uK -",XMSdx,XMSax,maxfree);
        
        XMScall(0x0900,maxfree);  /* allocate extended memory */
        
        
        if (XMSax == 0)
            break;

        totalallocated += maxfree;

        hTable[numhandles].handle = XMSdx;
        hTable[numhandles].size   = maxfree;
        
                         
    	if (verbose)
        	printf(" handle %x",hTable[numhandles].handle);

        XMScall(0x0c00,hTable[numhandles].handle);
        if (XMSax != 1)
            printf("$ERROR:\acan't lock - %02x", XMSbx & 0xff);
        else
            {
    	    if (verbose)
            	printf(" phys = %04x%04x ",XMSdx,XMSbx);
            XMScall(0x0d00,hTable[numhandles].handle);
            }
            
        
        if (verbose)
            printf("\n");
        
        }            
        
    if (verbose)
        printf("\ndone - total allocated %lu\n",totalallocated);


    /* now we have nearly every XMS memory allocated, fill it */

    for (i = 0; i < numhandles; i++)
        {
        if (verbose)
            printf("filling - handle %x, size %5uK\r",hTable[i].handle,hTable[i].size);
        
        for (offset = 0; offset < hTable[i].size; offset++)
            {
            fillMem(buff1,i,offset);

            if (XMSmove(hTable[i].handle, ((long)offset << 10), 
                    0,                (long)(void far*)&buff1,
                    1024))
				{
				ERROR("error in fill\n");
				goto error_fill;
				}                    
			/* hexd(buff1,16);
			exit(1);				*/
            }                
        }
error_fill:        
    if (verbose) printf("\n");

    /* now its filled, verify content */

    for (i = 0; i < numhandles; i++)
        {

	if (verbose)	
        	printf("verify handle %x, size %5uK\r",hTable[i].handle,hTable[i].size);
        
        for (offset = 0; offset < hTable[i].size; offset++)
            {
            fillMem(buff1,i,offset);

            if (XMSmove(
                    0,                (long)(void far*)&buff2,
                    hTable[i].handle, ((long)offset << 10), 
                    1024))
				{
				ERROR("error in verify offset %x size %x\n",offset , hTable[i].size);
				goto error_verify;
				}                    
            }                

        if (fmemcmp((void far *)buff1,(void far *) buff2, 1024))
            {
            ERROR("problems retrieving contents of handle %u = %x at offset %uK\n",
                        i,hTable[i].handle,offset);
			goto error_verify;
            }

            
        }
error_verify:
    if (verbose) printf("\n");


    /* now shift down all memory for all handles by 1 K */

    for (i = 0; i < numhandles; i++)
        {
	if (verbose)
            printf("shifting - handle %x, size %5uK\r",hTable[i].handle,hTable[i].size - 1);
        
        for (offset = 0; offset < hTable[i].size - 1; offset++)
            {
            fillMem(buff1,i,offset);

            if (XMSmove(hTable[i].handle, ((long)offset << 10), 
                    hTable[i].handle, ((long)(offset+1) << 10), 
                    1024))
				{
				ERROR("error in shift\n");
				goto error_shift;
				}                    
                    
            }                
        }       
error_shift:        
    if (verbose)
        printf("\n");

    
    /* now its shifted, verify content again */

    for (i = 0; i < numhandles; i++)
        {
	if (verbose)
            printf("verify handle %x, size %5uK\r",hTable[i].handle,hTable[i].size);
        
        for (offset = 0; offset < hTable[i].size - 1; offset++)
            {
            fillMem(buff1,i,offset+1);

            if (XMSmove(
                    0,                (long)(void far*)&buff2,
                    hTable[i].handle, ((long)offset << 10), 
                    1024))
				{
				ERROR("error in reverify\n");
				goto error_reverify;
				}                    
                    
            }                

        if (fmemcmp((void far *)buff1, (void far *)buff2, 1024))
            printf("problems after shift: handle %u = %x at offset %uK\n",
                        i,hTable[i].handle,offset);
        }
error_reverify:        
    if (verbose)
        printf("\n");
    


        

    /* on exit, we MUST release the memory again */
    /* and we do that in random order !          */
    
    __srand(ticks());

    if (verbose)
	printf("freeing handles - " );   
    for (i = 0; i < numhandles; )
        {
        int randomindex = __rand() % numhandles;
        
        if (hTable[randomindex].handle)
            {
            if (verbose)
                printf("%x ",hTable[randomindex].handle);
            XMScall(0x0a00,hTable[randomindex].handle);    
            hTable[randomindex].handle = 0;
            i++;
            }
        }
    if (verbose)
        printf("\n");

    
    XMScall(0x0800,0);  /* query free extended memory */

    freeMemAtEnd = XMSax;

    if (freeMemAtEnd != freeMemAtStart)
        ERROR("$ERROR$\a:free memory end %u != start %u\n",freeMemAtEnd,freeMemAtStart);

	endClock = ticks();

    if (totalallocated)
		if (verbose)
   		     printf("ms used per MB %lu\n", (ulong)(endClock - startClock)*51*1024/totalallocated );


	test_xms_realloc();


	if (errors_occurred)
	{
		printf("%d errors occurred\a",errors_occurred);
		if (!verbose)
			printf("Please re-run the HIMEM test in a verbose mode or from the commandline");
	}
	else
		printf ("Extended memory tested successfully\n");

    printf("Elapsed time : %u clock(s)\n", endClock - startClock );
	   	
   	return errors_occurred == 0;
}        


/*
	this should grap and kill each memory above of physical 64M
	
	reason: currently, EMM386 isn't yet able to handle > 64M
	so CURRENT strategy is: as soon as EMM386 is loded, every 
	XMS > 64M is allocated.
*/	


XMSKILL64()
{
#ifndef __KILL64
	printf("KILL64 not implemented\n");
#else	

	unsigned largestfree;
	unsigned handle;
	ulong 	 physoffset,physoffsetend;
	unsigned numhandles = 0;
	unsigned i;
	
	__int__(3);
	
	
	if (!XMScall(0,0))           /* get version number */
		{
		printf("no XMS handler detected\n");
		return 1;
		}

    for (numhandles = 0; numhandles < LENGTH(hTable); )
        {    
        XMScall(0x0800,0);  /* query free extended memory */
        
        largestfree = XMSax;


    	printf("free memory total %uK, largest %uk\n", XMSdx, largestfree);
    	
    	if (largestfree == 0)
    		break;


								/* allocate biggest chunk possible */    	
        XMScall(0x0900,largestfree);  /* allocate extended memory */
        
        if (XMSax == 0)
        	{
        	printf("OUTCH ??\a\n");	/* this should have succeeded */
            break;
            }       

		handle = XMSdx;            

        XMScall(0x0c00,handle);
        if (XMSax != 1)
        	{
            printf("$ERROR:\acan't lock - %02x\a\n", XMSbx & 0xff);
            break;
            }
            
        
		physoffset    = (((ulong)XMSdx)<<16) + XMSbx;
		physoffsetend = physoffset+(ulong)largestfree*1024-1;
		
        XMScall(0x0d00,handle);		/* unlock */
		

		printf(" handle %x at %lx..%lx\n",handle,physoffset, physoffsetend);


		if (physoffset >= 64l*1024*1024)
			{
					/* it's all above 64M - 'forget it' */
					
			continue;					
			}


        hTable[numhandles].handle = handle;		/* to be freed later */
        numhandles++;

		if (physoffsetend > 64l*1024*1024)
			{
					/* shrink it, so it doesn't run into 64M anymore */

			XMSbx = (unsigned)((64l*1024*1024 - physoffset) /1024);

			printf("shrinking to %uK\n",XMSbx);
			
			XMScall(0x0f00,handle);
			
			}
        }

	for ( i = 0; i < numhandles; i++)
		{
		printf("freeing handle %x\n", hTable[i].handle);
		XMScall(0x0a00,hTable[i].handle);    
		}

#endif
	return 0;
}




