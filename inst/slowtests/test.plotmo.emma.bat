@rem test.plotmo.emma.R: regression tests for emma with plotmo
@rem Stephen Milborrow, Shrewsbury Nov 2014

@echo === test.plotmo.emma ==============================================
@"\PROGRA~1\R\R-3.1.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotmo.emma.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.plotmo.emma.Rout:
@echo.
@tail test.plotmo.emma.Rout
@echo.
@exit /B 1
:good1
diff test.plotmo.emma.Rout test.plotmo.emma.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@rem @diffps -s Rplots.ps ..\..\.#\test-reference\test.plotmo.emma.save.ps
@exit /B 1
:good2
@rem test.plotmo.emma.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotmo.emma.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotmo.emma.Rout
@rm -f Rplots.ps
@exit /B 0
