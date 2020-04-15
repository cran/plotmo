@rem test.partdep.bat: partdep tests for plotmo and plotres

@echo test.partdep.bat
@"C:\PROGRA~1\R\R-3.6.3\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.partdep.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.partdep.Rout:
@echo.
@tail test.partdep.Rout
@echo test.partdep.R
@exit /B 1
:good1
mks.diff test.partdep.Rout test.partdep.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.partdep.save.ps
@exit /B 1
:good2
@rem test.partdep.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.partdep.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.partdep.Rout
@rm -f Rplots.ps
@exit /B 0
