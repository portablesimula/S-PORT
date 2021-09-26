echo off
if "%1=="0" goto OK
if "%1=="1" goto OK
echo ************************************************
echo * Compiling class osenv:                       *
echo *    osenv target debug-mode                   *
echo * where target is 0:no coprocessor, or 1:80x87 *
echo ************************************************
goto end
:OK
rem  ***  -P1  to interface correctly to Simula system
sim -P1 -Z%1 -d%2 osenv.def
:end
