@rem test.ltut.bat: test modified version of linmod example in
@rem                Friedrich Leisch "Creating R Packages: A Tutorial"

@"C:\PROGRA~1\R\R-3.2.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.ltut.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.ltut.Rout:
@echo.
@tail test.ltut.Rout
@echo test.ltut.R
@exit /B 1
:good1
diff test.ltut.Rout test.ltut.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.ltut.save.ps
@exit /B 1
:good2
@rem test.ltut.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.ltut.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.ltut.Rout
@rm -f Rplots.ps
@exit /B 0
