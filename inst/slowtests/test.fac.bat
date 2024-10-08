@rem test.fac.bat: test factor plotting in plotmo. This also tests swapxy, xflip, and yflip
@rem Stephen Milborrow, Berea Mar 2011

@echo test.fac.bat
@"C:\PROGRA~1\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.fac.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.fac.Rout:
@echo.
@tail test.fac.Rout
@echo test.fac.R
@exit /B 1
:good1
mks.diff test.fac.Rout test.fac.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.fac.save.ps
@exit /B 1
:good2
@rem test.fac.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.fac.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.fac.Rout
@rm -f Rplots.ps
@exit /B 0
