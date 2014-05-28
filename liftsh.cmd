@echo off

set SBT_LAUNCHER_PATH="project\sbt-launch-0.13.5.jar"
set SBT_LAUNCHER_SOURCE="http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.5/sbt-launch.jar"

if not exist %SBT_LAUNCHER_PATH% powershell -Command "(New-Object Net.WebClient).DownloadFile('%SBT_LAUNCHER_SOURCE%', '%SBT_LAUNCHER_PATH%')"

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
java %INTERNAL_OPTS% %LIFTSH_OPTS% -jar "%~dp0\%SBT_LAUNCHER_PATH%" %*
