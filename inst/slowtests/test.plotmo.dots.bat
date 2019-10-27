@rem test.plotmo.dots.R:  test handling of dots arguments

@echo test.plotmo.dots.bat
@"C:\PROGRA~1\R\R-3.6.1\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotmo.dots.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.plotmo.dots.Rout:
@echo.
@tail test.plotmo.dots.Rout
@echo test.plotmo.dots.R
@exit /B 1
:good1
mks.diff test.plotmo.dots.Rout test.plotmo.dots.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.plotmo.dots.save.ps
@exit /B 1
:good2
@rem test.plotmo.dots.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotmo.dots.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotmo.dots.Rout
@rm -f Rplots.ps
@exit /B 0
