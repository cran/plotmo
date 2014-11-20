@rem plotmo/inst/slowtests/make.bat

time /T
@call test.plotmo.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.non.earth.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.fac.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.center.bat
                        @if %errorlevel% NEQ 0 goto error
@echo.
@call test.plotmo.emma.bat
                        @if %errorlevel% NEQ 0 goto error
@rem for a thorough test, also run \b\earth\inst\slowtests\make.bat
@cd \b\earth\inst\slowtests
                        @if %errorlevel% NEQ 0 goto error
@call make.bat
                        @if %errorlevel% NEQ 0 goto error
@cd \b\plotmo\inst\slowtests
@goto done
:error
@echo ==== ERROR ====
:done
time /T
@exit /B  0
