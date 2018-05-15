@rem test.glmnet.bat: glmnet tests for plotmo and plotres

@"C:\PROGRA~1\R\R-3.5.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.glmnet.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.glmnet.Rout:
@echo.
@tail test.glmnet.Rout
@echo test.glmnet.R
@exit /B 1
:good1
diff test.glmnet.Rout test.glmnet.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.glmnet.save.ps
@exit /B 1
:good2
@rem test.glmnet.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.glmnet.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.glmnet.Rout
@rm -f Rplots.ps
@exit /B 0
