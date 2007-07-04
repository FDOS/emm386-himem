@echo off

:- batch file that is included in all other batch files for configuration

:-----------------------------------------------------------------------
:- NOTICE! You must edit and rename this file to CONFIG.BAT!
:-----------------------------------------------------------------------

:- Determine compiler(s) settings.
:- You REQUIRED to
:-  search for XASM	- and set the path to assembler (TASM, NASM)
:-  search for COMPILER	- and set the default compiler name
:-  search for BASE	- and set the path to (all) compiler(s)

:-----------------------------------------------------------------------
:- Assembler executable and options. NASM should not be protected mode
:- DJGPP version if you're using Windows NT/2k/XP to compile. Also:
:- NASM/DJGPP crashes when using protected mode Borland's MAKE.

set XASM=c:\bin\tasm /m/mx
set AFLAGS=/z/la/n

::set XASM=c:\bin\nasm16 -fobj
::set AFLAGS=

:-----------------------------------------------------------------------
:- COMPILER name.

:- Turbo C 2.01
set COMPILER=TC
:- Turbo C++ 1.01
::set COMPILER=TCPP
:- Turbo C++ 3.0
::set COMPILER=TCPP3
:- Borland C
::set COMPILER=BC
:- Microsoft C
::set COMPILER=MSC
:- Watcom C
::set COMPILER=WATCOM

:-----------------------------------------------------------------------
:- BASE path for compiler(s); all may be defined simultaneously.

set TC_BASE=c:\tc
::set TCPP_BASE=c:\tcpp
::set TCPP3_BASE=c:\tcpp3
::set BC_BASE=c:\bc
::set MSC_BASE=c:\msc
::set WATCOM=c:\watcom

:-----------------------------------------------------------------------
:- To make executable, compilers search linkers through PATH; if some
:- linkers not in PATH, uncomment OLDPATH and all required SET PATH.

::set OLDPATH=%PATH%
::set PATH=%TC_BASE%;%PATH%
::set PATH=%TCPP_BASE%\bin;%PATH%
::set PATH=%TCPP3_BASE%\bin;%PATH%
::set PATH=%BC_BASE%\bin;%PATH%
::set PATH=%MSC_BASE%\bin;%PATH%
::set PATH=%WATCOM%\binw;%PATH%

:- MSC searches libraries only through LIB variable.

::set LIB=%MSC_BASE%\lib

:-----------------------------------------------------------------------
:- Define which LINKer to use
:- OR it will be determined AUTOMATICALLY.

:- Turbo Link
::set XLINK=tlink /3/c/m/s/l
:- Microsoft Link
::set XLINK=link /ONERROR:NOEXE /nologo
:- WATCOM Link (wlinker is a batch file calling ms2wlink and wlink)
::set XLINK=wlinker /nologo

:-----------------------------------------------------------------------
:- Define which LIBrarian to use
:- OR it will be determined AUTOMATICALLY.

:- Turbo Lib
::set XLIB=tlib
::set LIBCMDTAIL=
:- Microsoft Lib
::set XLIB=lib /nologo
::set LIBCMDTAIL=;
:- WATCOM Lib
::set XLIB=wlib -q
::set LIBCMDTAIL=

:-----------------------------------------------------------------------
:- Define which MAKE to use
:- OR it will be determined AUTOMATICALLY.

:- Borland MAKE
::set XMAKE=make
::set XMAKE=maker -S
:- Watcom MAKE in MS mode
::set XMAKE=wmake /ms
:- Microsoft MAKE
::set XMAKE=nmake /nologo
::set XMAKE=nmaker /nologo

:-----------------------------------------------------------------------
:- Packer executable and options.
:- Remain commented if you don't want to use it.

::set XPACK=upx --8086 --brute

:-----------------------------------------------------------------------
:- Target CPU (default is 86). Note: TC doesn't support 386 CPU.

::set XCPU=86
::set XCPU=186
::set XCPU=386

:- Extra compiler flags,
:- such as -DDEBUG : extra DEBUG output
:-         -DDOSEMU : printf output goes to dosemu log

::set CFLAGS=-DDEBUG

:-----------------------------------------------------------------------

if "%dos4g%" == "" set dos4g=quiet
