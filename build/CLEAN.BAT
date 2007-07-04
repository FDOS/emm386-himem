@echo off

:- batch file to clean and clobber everything

if "%1" == "" %0 clean
goto %1

:clean
:clobber
if not exist config.bat echo Copy CONFIG.B to CONFIG.BAT and edit it to reflect your setup!
if not exist config.bat goto end

call config.bat
call defaults.bat
if not "%XMAKE%" == "" call %XMAKE% %1

:end
defaults.bat clean
