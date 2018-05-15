@rem test.plotmo.args.bat: test dot and other argument handling in plotmo

@"C:\PROGRA~1\R\R-3.5.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotmo.args.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.plotmo.args.Rout:
@echo.
@tail test.plotmo.args.Rout
@echo test.plotmo.args.R
@exit /B 1
:good1
diff test.plotmo.args.Rout test.plotmo.args.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.plotmo.args.save.ps
@exit /B 1
:good2
@rem test.plotmo.args.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotmo.args.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotmo.args.Rout
@rm -f Rplots.ps
@exit /B 0
