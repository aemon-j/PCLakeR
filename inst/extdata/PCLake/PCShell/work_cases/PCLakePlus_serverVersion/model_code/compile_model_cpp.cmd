rem # this batchfile compiles 'model.cpp' with the GCC compiler (which can be downloaded by downloading RTools at http://cran.r-project.org/bin/windows/Rtools/ )
set PATH=c:\rtools40\usr\bin;c:\rtools40\mingw64\bin;c:\PROGRA~1\R\R-4.1.0\bin
del model.o            
del model.dll
R CMD SHLIB model.cpp >info.txt 2>error.txt
pause