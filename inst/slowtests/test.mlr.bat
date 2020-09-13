@rem test.mlr.bat: mlr tests for plotmo and plotres

@echo test.mlr.bat
@"C:\PROGRA~1\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.mlr.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.mlr.Rout:
@echo.
@tail test.mlr.Rout
@echo test.mlr.R
@exit /B 1
:good1
mks.diff test.mlr.Rout test.mlr.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.mlr.save.ps
@exit /B 1
:good2
@rem test.mlr.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.mlr.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.mlr.Rout
@rm -f Rplots.ps
@exit /B 0
