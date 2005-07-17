/* 
	USEFUL.H

	some definitions, useful for generic (RTL free) programs
		
*/	

#pragma warn -asc

typedef unsigned char uchar;
typedef unsigned int   uint;
typedef unsigned short ushort;
typedef unsigned long ulong;


#define NULL 0
#define TRUE 1
#define FALSE 0

#define MK_FP(seg,ofs)        ((void far *)(((ulong)(seg)<<16)|(ushort)(ofs)))
#define FP_SEG(fp)            ((ushort)((ulong)(void far *)(fp)>>16))
#define FP_OFF(fp)            ((ushort)(fp))


/* from FreeDOS kernel PRF.C */

extern int printf(char *,...); 
extern void hexd(char *title,void far *p,int numBytes);


/* some utility functions */

int  fmemcmp(char far *s1, char far *s2, int count);
void fmemset(void far *s1, char c, int count);
void fmemcpy(char far *s1,char far *s2,int len);
void Bye();
char far *skipWhite(char far *s);
int  toupper (unsigned c);

#define isWhite(c) ((c) == ' ' || (c) == '\t')

/* int GetValue(char far *commandline,int base);	*/
long GetValue(char far *commandline,int base, char usesuffix);
int FindCommand(char far *commandline, char *searchstring, char far **found);


/* your code must provide this function */

#define EXECMODE_SYS 0
#define EXECMODE_EXE 1

int TheRealMain(int mode, char far *commandline);


/* PRF.C */
int fstrlen (char far * s);        /* don't want globals.h, sorry */



