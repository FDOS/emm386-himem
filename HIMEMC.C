/*
	HIMEMC.C
	
	commandline handling for HIMEM
	
	this stuff is copyrighted (c) by tom ehlert
	
	given to the FreeDOS project under GPL license	
	
*/

#define PROGRAM "HIMEM"

#include "useful.h"


extern char   far init_message[];
extern char   far vinit_message[];
extern char   far copyright[];
extern ushort far xms_num_handles;	/* default 72 */
extern char   far startup_verbose;
extern char   far xms_logging_enabled;
extern char   far no_above_16;
extern char   far x_option;
extern char   far x2max32;
extern char   far alwayson_set,bios_set,ps2_set,fast_set,port92_set,kbc_set;
extern ulong far xms_max;
extern ushort far hma_min;


/* called on startup.
	handle commandline "NUMHANDLES=" ...
	...
	mode = 0 if called as driver
	mode = 1 if called as EXE
	
	return: 0         - everything fine
	        errorcode - exit code/abort driver
*/ 

int TheRealMain(int mode, char far *commandline)
{
	char far *found;
	int int15 = 0;
	
	if (mode == EXECMODE_EXE)
		startup_verbose = 1;
    
                             
	printf("%Fs", init_message); 
	printf(" ["__DATE__ "] ");
	printf("%Fs",copyright); 
	printf("\n");

	/******* commandline handling **********/
	
	if (FindCommand(commandline, "/?", &found) )
		{
		if ( mode == EXECMODE_SYS )	/* for crazy callers */
		    printf ("Option /? cannot be used as a device driver.\nPlease run HIMEM /? from the commandline");
		}

	if (FindCommand(commandline, "/TESTMEM:OFF", &found) ||
		FindCommand(commandline, "/?", &found))
		{
			/* do nothing */
		}

	if (FindCommand(commandline, "/VERBOSE", &found) )
		{ 
		startup_verbose = 1;
		}              

		
	if (FindCommand(commandline, "/LOG", &found) )
		{ 
		xms_logging_enabled = 1;
		}


	if (startup_verbose)
		{
		printf("%Fs\n",vinit_message);
		}

	if (FindCommand(commandline, "/NUMHANDLES=", &found) )
		{ 
		xms_num_handles = GetValue(found,10,FALSE);
		if (startup_verbose)
			printf("selected num_handles=%d\n",xms_num_handles);

		if (xms_num_handles < 8)
			{
			printf("HIMEM:NUMHANDLES must be >= 8, corrected\n");
			xms_num_handles = 8;
			}			
			
		if (xms_num_handles > 128)
			{
			printf("HIMEM:NUMHANDLES must be <= 128, corrected\n");
			xms_num_handles = 128;
			}			
			
		}
		
		
/*	if (FindCommand(commandline, "/TESTMEM:ON", &found) ||
		FindCommand(commandline, "/TESTMEM:OFF", &found) )
		{
		if (startup_verbose)
			printf("/TESTMEM:ON|OFF not implemented - ignored\n");
		}
*/
		

	if (FindCommand(commandline, "/NOABOVE16", &found) )
		{
			no_above_16 = 1;
		}

	/* must come before /X option check */
	if (FindCommand(commandline, "/X2MAX32", &found) )
		{
			x2max32 = 1;
		}

	if (FindCommand(commandline, "/X", &found) )
		{
			x_option = 1;
		}

	if (FindCommand(commandline, "/METHOD:ALWAYSON", &found) )
		{
			alwayson_set = 1;
		}

	if (FindCommand(commandline, "/METHOD:BIOS", &found) )
		{
			bios_set = 1;
		}

	if (FindCommand(commandline, "/METHOD:FAST", &found) )
		{
			fast_set = 1;
		}

	if (FindCommand(commandline, "/METHOD:PS2", &found) )
		{
			ps2_set = 1;
		}

	if (FindCommand(commandline, "/METHOD:PORT92", &found) )
		{
			port92_set = 1;
		}

	if (FindCommand(commandline, "/METHOD:KBC", &found) )
		{
			kbc_set = 1;
		}

	if (FindCommand(commandline, "/MAX=", &found) )
		{ 
		xms_max = GetValue(found,10,TRUE);
		if (startup_verbose)
			printf("Maximum XMS: %luK\n",xms_max);
		}
		

	if (FindCommand(commandline, "/HMAMIN=", &found) )
		{ 
		hma_min = GetValue(found,10,TRUE);

		if (startup_verbose)
			printf("Minimum HMA that has to be requested: %uK\n",hma_min);

		if (hma_min > 64)
			{
			printf("HIMEM:HMAMIN must be <= 64, corrected\n");
			hma_min = 64;
			}			

		}
		
	if (FindCommand(commandline, "/INT15=", &found) )
		{ 
		int15 = (int)GetValue(found,16,FALSE);
		
						/* you probably intended this something to do
						   so we warn (beep) */
		
		printf("\aHIMEM:/INT15=%x - not implemented\n",int15);
		}

	if (FindCommand(commandline, "/TESTMEM:ON", &found) || FindCommand(commandline, "/TEST", &found))
		{
		extern XMSTESTmain(char);
		
		XMSTESTmain(startup_verbose);
		
		return 0;
		}


	if (FindCommand(commandline, "/KILL64", &found))
		{
		extern XMSKILL64(void);
		
		XMSKILL64();
		
		return 0;
		}


	/******* commandline handling done, are there remainders **********/

	commandline = skipWhite(commandline);

	if (*commandline)
		printf("ignored commandline <%Fs>\n", commandline);



	if (mode == EXECMODE_EXE)
		{
							/* called as exe from commandline 
								do :
									usage info
									status report
									show memory statistic
									...
							*/
							

		printf("Extended memory host for FreeDOS (coordinates the usage of XMS and HMA)\n"
		       "HIMEM is a device driver that is loaded in CONFIG.SYS.\n"
		       "Please place DEVICE=HIMEM.EXE before any driver using XMS.\n\n"
		       "HIMEM [/MAX=####] [/METHOD:xxx] [/HMAMIN=n] [/NUMHANDLES=m]\n"
		       " [/TESTMEM:ON|OFF] [/VERBOSE] [/NOABOVE16] [/X] [/LOG]\n\n"
		       "  /MAX=#####      limit memory controlled by HIMEM to #####K\n"
		       "  /METHOD:xxx     Specifies the method to be used for A20 handling.\n"
		       "                  Possible values for xxx:\n"
		       "                  ALWAYSON    Assume that A20 line is permanently ON\n"
		       "                  BIOS        Use BIOS to toggle the A20 line\n"
		       "                  FAST        Use port 92h, bypass INT 15h test\n"
		       "                  PS2         Use port 92h, bypass PS/2 test\n"
		       "                  KBC         Use the keyboard controller\n"
		       "                  PORT92      Use port 92h always\n"
		       "  /HMAMIN=n       Specifies minimum number of Kbs of HMA that a program\n"
		       "                  must request to gain access to the HMA (default: 0Kb)\n"
		       "  /NUMHANDLES=m   Specifies number of XMS handles available (def: 72)\n"
		       "  /TESTMEM:ON|OFF Performs or skips an extended memory test (def: OFF)\n"
		       "  /VERBOSE        Gives extra information\n" 
		       "  /NOABOVE16      Do not use INT 15h function E801h to detect >64M\n"
		       "  /X              Do not use INT 15h function E820h to detect >64M\n"
			   "  /X2MAX32        Limit XMS 2.0 free/available memory report to 32M-1K\n"
		       "  /LOG            Logs the driver activity to a log file\n"
		       
				);				
				

		return 1;
		}											



	return 0;			/* driver can't fail */

}

 