@rem test.glmnetUtils.bat: glmnetUtils tests for plotmo and plotres

@"C:\PROGRA~1\R\R-3.5.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.glmnetUtils.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.glmnetUtils.Rout:
@echo.
@tail test.glmnetUtils.Rout
@echo test.glmnetUtils.R
@exit /B 1
:good1
diff test.glmnetUtils.Rout test.glmnetUtils.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.glmnetUtils.save.ps
@exit /B 1
:good2
@rem test.glmnetUtils.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.glmnetUtils.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.glmnetUtils.Rout
@rm -f Rplots.ps
@exit /B 0
