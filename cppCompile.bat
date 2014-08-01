call vcvars32.bat
cl.exe /EHsc %1.cpp /Fo%1.obj /Fe%1.exe
@ECHO OFF
set callexe=%1
set callexe=%callexe:/=\%
@ECHO ON
%callexe%.exe
@ECHO OFF
exit