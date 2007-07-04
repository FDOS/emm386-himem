@echo off

if "%1" == "clean" goto clean

:-----------------------------------------------------------------------

set BASE=
if "%COMPILER%" == "TC"     set BASE=%TC_BASE%
if "%COMPILER%" == "TCPP"   set BASE=%TCPP_BASE%
if "%COMPILER%" == "TCPP3"  set BASE=%TCPP3_BASE%
if "%COMPILER%" == "BC"     set BASE=%BC_BASE%
if "%COMPILER%" == "WATCOM" set BASE=%WATCOM%
if "%COMPILER%" == "MSC"    set BASE=%MSC_BASE%

if "%COMPILER%" == ""   echo COMPILER variable MUST be defined!
if "%BASE%"     == ""   echo Compiler BASE path MUST be defined!
if "%BASE%"     == ""   goto error
if not exist %BASE%\nul echo Compiler BASE path not exist!
if not exist %BASE%\nul goto error

:-----------------------------------------------------------------------

if not "%XLINK%" == "" goto skip_link

set XLINK=%BASE%\bin\tlink /3/c/m
if "%COMPILER%" == "TC"     set XLINK=%BASE%\tlink /3/c/m
if "%COMPILER%" == "WATCOM" set XLINK=wlinker /nologo
if "%COMPILER%" == "MSC"    set XLINK=%BASE%\bin\link /ONERROR:NOEXE /batch

echo Linker is %XLINK%
if "%XLINK%" == "" goto error

:skip_link

:-----------------------------------------------------------------------

if not "%XLIB%" == "" goto skip_lib

set XLIB=%BASE%\bin\tlib
set LIBCMDTAIL=
if "%COMPILER%" == "TC"     set XLIB=%BASE%\tlib
if "%COMPILER%" == "WATCOM" set XLIB=%BASE%\binw\wlib -q
if "%COMPILER%" == "MSC"    set XLIB=%BASE%\bin\lib /nologo
if "%COMPILER%" == "MSC"    set LIBCMDTAIL=;

echo Librarian is %XLIB%
if "%XLIB%" == "" goto error

:skip_lib

:-----------------------------------------------------------------------

if not "%XMAKE%" == "" goto skip_make

set XMAKE=%BASE%\bin\make
if "%COMPILER%" == "TC"     set XMAKE=%BASE%\make
if "%COMPILER%" == "WATCOM" set XMAKE=%BASE%\binw\wmake /ms /h
if "%COMPILER%" == "MSC"    set XMAKE=%BASE%\bin\nmake /nologo

echo Maker is %XMAKE%
if "%XMAKE%" == "" goto error

:skip_make

:-----------------------------------------------------------------------

goto end

:error
echo Unable to set necessary environment variables!

:-----------------------------------------------------------------------

:clean

set XASM=
set AFLAGS=
set COMPILER=
set TC_BASE=
set TCPP_BASE=
set TCPP3_BASE=
set BC_BASE=
set MSC_BASE=
set BASE=
set XLINK=
set XLIB=
set LIBCMDTAIL=
set XMAKE=
set XPACK=
set XCPU=
set CFLAGS=

if not "%OLDPATH%" == "" set PATH=%OLDPATH%
set OLDPATH=

:end
