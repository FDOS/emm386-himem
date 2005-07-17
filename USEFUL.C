/*
	useful.c
	
	Copyright (c) by tom ehlert 2001-2004 - all rights reserved

	Licensed under the Artistic License version
	
	please see LICENSE.TXT for details

*/
#include "useful.h"


/**************** utility functions, missing from TC lib :-( *********************/

int fmemcmp(char far *s1, char far *s2, int count)
{
	for ( ; count; s1++,s2++,count--)
		if (*s1 != *s2)
			return *s1-*s2;
			
	return 0;
}				
void fmemset(void far *s1, char c, int count)
{
	char far *s = (char far *)s1;
	for ( ; count; count--)
		*s++ = c;
}	
/*
int fstrlen(char far *s)
{
	char far *o = s;
	while (*s)
		s++;
	return s - o;		
}			
*/
int fmemicmp(char far *s1,char far *s2,int len)
{
	int c;
	
	for ( ; len; s1++,s2++,len--)
		if ((c = toupper(*s1) - toupper(*s2)) != 0)
			return c;
	return 0;			
}

void fmemcpy(char far *s1,char far *s2,int len)
{
	for ( ; len; s1++,s2++,len--)
		*s1 = *s2;
}

int fstrlen (char far * s)        /* don't want globals.h, sorry */
{
    int i = 0;

    while (*s++)
       i++;

    return i;
}



void Bye()
{
	asm mov ax,0x4c01;
	asm int 0x21;
}

char far *skipWhite(char far *s)
{
	while (isWhite(*s))
		s++;
	return s;		
}            

int toupper (unsigned c)
{
	if (c >= 'a' && c <= 'z')
		return c - 0x20;

	return c;
}			

int FindCommand(char far *commandline, char *searchstring, char far **found)
{
	int searchlen = fstrlen((char far *)searchstring);

	for ( ; *commandline; commandline++)
		{
		if (fmemicmp(commandline,(char far *)searchstring,searchlen) == 0)
			{
            *found = commandline;
            
            						/* now remove found string */
			fmemcpy( commandline, commandline+searchlen, 
								fstrlen(commandline+searchlen) +1); 
            
			return TRUE;            
			}
		}
	return FALSE;
}

/* return greater than int values
int GetValue(char far *commandline,int base)
*/
/* ASM(2004/08/24): the suffix assumes that the base is in kilobytes */
long GetValue(char far *commandline,int base, char usesuffix)
{
	int len = 0 /*, result = 0 */,digit;
	long result = 0L;
	
	for(;; len++)
		{
		digit = toupper(commandline[len]);

		if (digit >= '0' && digit <= '9')
			digit = digit - '0';
		else if (digit >= 'A')
			digit = digit - 'A' + 10;
		else
			break;

		if (digit >= base)
			break;			
		result = result * base + digit;			
		}				
			
        if (usesuffix)
	switch (toupper(commandline[len]))
	{
		case 'M': result *= 1024;
		case 'K': commandline[len]=' ';
		          break;
		case 'G': result *= 1024*1024;
		         commandline[len]=' ';
	}

	
            						/* now remove found string */
	fmemcpy( commandline, commandline+len, 
								fstrlen(commandline+len) +1); 

	return result;	
}


/*
	C-Entry point to either EXE or SYS startup code
	converst the commandline to something useful (0-terminated,
	filename skipped)
	and calls TheRealMain
*/



/* EXE entry to this code */

struct DOScommandline {
	unsigned char count;
	char cmd[0x7f];
	};

int far startup_exe(struct DOScommandline far *cmdline)
{   
/*  	printf("executing the executable part of XXXXXdriver\n"); */

	if (cmdline->count >= 0x7f) cmdline->count = 0x7f;

	cmdline->cmd[cmdline->count] = 0;	
		
    return TheRealMain(EXECMODE_EXE,cmdline->cmd); 
}

/* driver entry to this code */

int far startup_driver(char far *cmdLine)
{
	char far *cmdstart = 0;
	char cmdsaved = 0;
	int retval;
	
/*  	printf("executing the driver part of XXXXXdriver\n");

    printf("'real' commandline is '%Fs'\n",cmdLine); */

						/* convert the commandline to 'C' */	
	for (;; cmdLine++)
		{
    	switch(*cmdLine)
    		{
    		case 0x00:
    		case 0x0a:
    		case 0x0d:
    			cmdsaved = *cmdLine;
    			*cmdLine = 0;
    			goto done;

			case ' ':
			case '\t':
				if (cmdstart == 0)
					cmdstart = cmdLine;
				break;
			}
		}

done:							    			
	if (cmdstart == 0)
		{
		/* printf("no commandline arguments\n"); */
		cmdstart = "";
		}
		
    
    retval = TheRealMain(EXECMODE_SYS, cmdstart); 
    
    if (cmdsaved)
    	*cmdLine = cmdsaved;
    

	return retval;    
}


/* 
	StartupLogging()
	
	if on startup Ctrl+LeftShift is pressed, return TRUE.
	
	this shall have the same effect as /VERBOSE, but 
	can be enabled dynamically, without editing CONFIG.SYS
*/
int StartupLogging()
{
	if ((*(char far *)MK_FP(0x40,0x17) & 0x06) == 0x06)
		return TRUE;
	else
		return FALSE;		
}

	
	
	
	
	