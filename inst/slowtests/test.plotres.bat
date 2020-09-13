@rem test.plotres.bat: test plotres

@echo test.plotres.bat
@"C:\PROGRA~1\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.plotres.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.plotres.Rout:
@echo.
@tail test.plotres.Rout
@echo test.plotres.R
@exit /B 1
:good1
mks.diff test.plotres.Rout test.plotres.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.plotres.save.ps
@exit /B 1
:good2
@rem test.plotres.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.plotres.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.plotres.Rout
@rm -f Rplots.ps
@exit /B 0
