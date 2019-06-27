@rem test.pre.bat: pre tests for plotmo and plotres

@echo test.pre.bat
@"C:\PROGRA~1\R\R-3.6.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.pre.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.pre.Rout:
@echo.
@tail test.pre.Rout
@echo test.pre.R
@exit /B 1
:good1
mks.diff test.pre.Rout test.pre.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.pre.save.ps
@exit /B 1
:good2
@rem test.pre.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.pre.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.pre.Rout
@rm -f Rplots.ps
@exit /B 0
