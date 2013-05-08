@echo off

@REM Internal options, always specified
set INTERNAL_OPTS=-Dfile.encoding=UTF-8 -Xmx768m -noverify -XX:ReservedCodeCacheSize=256m -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:MaxPermSize=512m

@REM Add 64bit specific option
java -version 2>&1 | find "64-Bit" >nul:
if not errorlevel 1 (
  set INTERNAL_OPTS=%INTERNAL_OPTS% -XX:+UseCompressedOops -XX:ReservedCodeCacheSize=128m
)

@REM Default options, if nothing is specified
set DEFAULT_OPTS=

if "%LIFTSH_OPTS%"=="" (
  set LIFTSH_OPTS=%DEFAULT_OPTS%
)

@REM Call with INTERNAL_OPTS followed by LIFTSH_OPTS (or DEFAULT_OPTS). java always takes the last option when duplicate.
java %INTERNAL_OPTS% %LIFTSH_OPTS% -jar "%~dp0\project\sbt-launch-0.12.1.jar" %*
