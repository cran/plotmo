@rem plotmo/inst/slowtests/make.bat

time /T
@call test.plotmo.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.degree.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.plotmo.x.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.printcall.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.plotmo.dots.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.plotmo3.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.plotmo.args.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.non.earth.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.caret.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.fac.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.center.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.emma.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.plotres.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.glmnet.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.glmnetUtils.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.gbm.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.pre.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.ltut.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.linmod.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.dots.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.partdep.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.c50.bat
                        @if %errorlevel% NEQ 0 goto error
@rem for a thorough test, we also run \a\r\earth\inst\slowtests\make.bat
@cd \a\r\earth\inst\slowtests
                        @if %errorlevel% NEQ 0 goto error
@call make.bat
                        @if %nerrorlevel% NEQ 0 goto error
@cd \a\r\plotmo\inst\slowtests
@goto done
:error
@echo ==== ERROR ====
:done
time /T
@exit /B  0
