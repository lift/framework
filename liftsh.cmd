@echo off

@REM Internal options, always specified
set INTERNAL_OPTS=-Dfile.encoding=UTF-8 -Xss8M -Xmx1G -noverify -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:MaxPermSize=512M

@REM Default options, if nothing is specified
set DEFAULT_OPTS=-Dsbt.intransitive=true

if "%LIFTSH_OPTS%"=="" (
  set LIFTSH_OPTS=%DEFAULT_OPTS%
)

@REM Call with INTERNAL_OPTS followed by LIFTSH_OPTS (or DEFAULT_OPTS). java aways takes the last option when duplicate.
java %INTERNAL_OPTS% %LIFTSH_OPTS% -jar "%~dp0\project\sbt-launch-0.7.7.jar" %*
