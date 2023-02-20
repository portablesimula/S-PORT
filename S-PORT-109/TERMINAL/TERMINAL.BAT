echo off
if "%1=="0" goto OK
if "%1=="1" goto OK
echo ************************************************
echo * Compiling class terminal:                    *
echo *    terminal target debug-mode                *
echo * where target is 0:no coprocessor, or 1:80x87 *
echo ************************************************
goto end
:OK
rem *** -P1 to set correct interface to Simula system
sim -P1 -Z%1 -d%2 terminal.def
:end
