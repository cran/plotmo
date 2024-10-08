@rem test.non.earth.bat: test plotmo on non-earth models
@rem Stephen Milborrow, Basley KwaZulu-Natal Mar 2011

@echo test.non.earth.bat
@"C:\PROGRA~1\R\R-4.4.1\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.non.earth.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.non.earth.Rout:
@echo.
@tail test.non.earth.Rout
@echo test.non.earth.R
@exit /B 1
:good1
mks.diff test.non.earth.Rout test.non.earth.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.non.earth.save.ps
@exit /B 1
:good2
@rem test.non.earth.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.non.earth.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.non.earth.Rout
@rm -f Rplots.ps
@exit /B 0
