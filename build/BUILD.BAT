@echo off

:- batch file to build everything

:-----------------------------------------------------------------------
:- Syntax: BUILD [-r] [msc|wc|tc|tcpp] [86|186|386] [debug]
:- option case is significant !!
:-----------------------------------------------------------------------

if not exist config.bat echo Copy CONFIG.B to CONFIG.BAT and edit it to reflect your setup!
if not exist config.bat goto abort

if "%1" == "-r" call clobber.bat
if "%1" == "-r" shift

call config.bat

:-----------------------------------------------------------------------
:- Following is command line handling.
:- Options on the commandline overwrite CONFIG.BAT settings.
:-----------------------------------------------------------------------

:loop_commandline

if "%1" == "msc"   set COMPILER=MSC
if "%1" == "wc"    set COMPILER=WATCOM
if "%1" == "tc"    set COMPILER=TC
if "%1" == "tcpp"  set COMPILER=TCPP

if "%1" == "86"    set XCPU=86
if "%1" == "186"   set XCPU=186
if "%1" == "386"   set XCPU=386

if "%1" == "debug" set CFLAGS=%CFLAGS% -DDEBUG

shift
if not "%1" == "" goto loop_commandline

call defaults.bat
if "%XMAKE%" == "" goto abort

:-----------------------------------------------------------------------
:- finally - we are going to compile
:-----------------------------------------------------------------------

:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
:- Currently (temporarily) build subsystem placed into subdirectory,
:- so for compilation copy all sources here...
copy ..\*.asm>nul
copy ..\*.c  >nul
copy ..\*.h  >nul
:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo.
call %XMAKE% all
:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
:- ...and remove them after compilation.
if exist *.asm del *.asm>nul
if exist *.c   del *.c  >nul
if exist *.h   del *.h  >nul
:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if errorlevel 1 goto abort

:- if you like, put finalizing commands into build2.bat

if exist build2.bat call build2.bat

echo.
echo Processing is done.
goto end

:-----------------------------------------------------------------------

:abort
echo Compilation aborted!

:end
defaults.bat clean
