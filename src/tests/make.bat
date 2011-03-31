@rem plotmo/src/tests/make.bat

time /T
@call test.plotmo.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.non.earth.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.fac.bat
                        @if %errorlevel% NEQ 0 goto error
@cd \a\earth\src\tests
                        @if %errorlevel% NEQ 0 goto error
@rem for a thorough test, run \a\earth\src\tests\make.bat
@call make.bat
@rem @call test.earth.glm.bat
                        @if %errorlevel% NEQ 0 goto error
@cd \a\plotmo\src\tests
@goto done
:error
@echo ==== ERROR ====
:done
@rm -f ../earth_res.rc ../Makedeps
@rm -f test.*.pdf
time /T
@exit /B  0
