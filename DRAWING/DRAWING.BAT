echo off
if "%1"=="0" goto OK
if "%1"=="1" goto OK
echo ************************************************
echo * Compiling class drawing:                     *
echo *    drawing  [target] {debug-mode}            *
echo * where target is 0:no coprocessor, or 1:80x87 *
echo *                                              *
echo * Test class drawing: compile e.g. drawglin :  *
echo *    sim -Z%1 -imemdraw.obj drawglin.sim       *
echo * and execute drawglin (answer 14 or 3).       *
echo *                                              *
echo * You may wish to incorporate memdraw.obj in   *
echo * e.g. SDx109EI.LIB (x is 0 and/or 7) by       *
echo *    lib sd7109ei.lib +memdraw.obj;            *
echo * - then you do not need '-imemdraw.obj'       *
echo ************************************************
goto end
:OK
rem ***  -P1 to ensure correct interfacing  ***
sim -P1 -Z%1 -d%2 drawing.def
:end
