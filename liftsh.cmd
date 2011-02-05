@echo off

set DEFAULT_OPTS="-Dfile.encoding=UTF-8 -Xss8M -Xmx1G -noverify -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M -Dfile.encoding=utf8 -Dsbt.intransitive=true"

if "%LIFTSH_OPTS%"=="" (
  set LIFTSH_OPTS=DEFAULT_OPTS
)

java "%LIFTSH_OPTS%" -jar "%~dp0\project\sbt-launch-0.7.5.RC0.jar" %*
