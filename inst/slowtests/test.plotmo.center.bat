@rem test.plotmo.center.bat: test plotmo's center and ndiscrete args
@rem Stephen Milborrow, Berea Apr 2011

@echo === test.plotmo.center ===============================================
@"\PROGRA~1\R\R-3.0.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotmo.center.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.plotmo.center.Rout:
@echo.
@tail test.plotmo.center.Rout
@echo.
@exit /B 1
:good1
diff test.plotmo.center.Rout test.plotmo.center.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.plotmo.center.save.ps
@exit /B 1
:good2
@rem test.plotmo.center.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotmo.center.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotmo.center.Rout
@rm -f Rplots.ps
@exit /B 0
