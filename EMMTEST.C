/* emmtest.c
*/

#include <stdio.h>
#include <stdlib.h>
#include <dos.h>


typedef unsigned int uchar;
typedef unsigned int uint;
typedef unsigned long ulong;

union REGS ir;

int emmcall(uint function)
{

	ir.h.ah = (char)function;

	int86(0x67,&ir,&ir);


	if (ir.h.ah != 0) printf("error function %x - err %x\n",function, ir.h.ah);

	return ir.h.ah;
}      

int fmemcmp(char far *s1, char far *s2, int count)
{
	for ( ; count; s1++,s2++,count--)
		if (*s1 != *s2)
			return *s1-*s2;
			
	return 0;
}				

main()
{
	/* check install state */

/* this is not correct code, per EMS spec only the segment is used
   with a fixed offset of 10 (0x0a)
	char far *p = (char far *)(long)getvect(0x67);

	if (p == NULL) printf("no int67"),exit(1);
*/
	char far * p;
	unsigned long seg = *(int far *)(0x67 * 4 + 2);
	if (seg == NULL)
	{
		printf("no int67");
		exit(1);
	}
	p = (char far *)(seg << 16);

	if (fmemcmp(p+10, "EMMXXXXX0",3) != 0) 
		{
		printf("no emm386");
		if (fmemcmp(MK_FP(FP_SEG(p),10), "EMMXXXX0",8) != 0) 
			printf("really no emm386"),exit(1);
		}			
		
	
	

	emmcall(0x46);
/*	printf("version %x\n",ir.h.al);	*/
	printf("version %d.%d\n",ir.h.al >> 4, ir.h.al & 0x0f);

	emmcall(0x41);
	printf("page frame at %x\n",ir.x.bx);
	
	emmcall(0x42);
	printf("total pages %x(%dMB), free %x(%dMB)\n",ir.x.dx,ir.x.dx/(1024/16),ir.x.bx,ir.x.bx/(1024/16));

	emmcall(0x4b);
	printf("emm handles %x\n",ir.x.bx);
		

	
	return 0;
}

