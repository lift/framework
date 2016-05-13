#!/bin/sh

# Make sure to change the name of the launcher jar and the source when bumping sbt version
# so that the existence test below fails and we download the new jar.
SBT_LAUNCHER_PATH="project/sbt-launch-0.13.8.jar"
SBT_LAUNCHER_SOURCE="https://dl.bintray.com/sbt/native-packages/sbt/0.13.8/sbt-0.13.8.tgz"

# Download the sbt launcher on-the-fly if it's not already in the repository.
if test ! -f $SBT_LAUNCHER_PATH; then
  BASEDIR=$(dirname $SBT_LAUNCHER_PATH)
  echo "Downloading sbt launcher..."
  curl -L -o ${SBT_LAUNCHER_PATH}.tgz ${SBT_LAUNCHER_SOURCE}
  tar xf $SBT_LAUNCHER_PATH.tgz -C $BASEDIR
  mv $BASEDIR/sbt/bin/sbt-launch.jar $SBT_LAUNCHER_PATH
  rm -rf $BASEDIR/sbt
fi

# Load custom liftsh config
if test -f ~/.liftsh.config; then
  . ~/.liftsh.config
fi

# Internal options, always specified
INTERNAL_OPTS="-Dfile.encoding=UTF-8 -Xmx1768m -noverify -XX:ReservedCodeCacheSize=296m -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:MaxPermSize=812m"

# Add 64bit specific option
exec java -version 2>&1 | grep -q "64-Bit" && INTERNAL_OPTS="${INTERNAL_OPTS} -XX:+UseCompressedOops -XX:ReservedCodeCacheSize=328m"

# Default options, if nothing is specified
DEFAULT_OPTS=""

cd `dirname $0`

# Call with INTERNAL_OPTS followed by LIFTSH_OPTS (or DEFAULT_OPTS). java always takes the last option when duplicate.
exec java ${INTERNAL_OPTS} ${LIFTSH_OPTS:-${DEFAULT_OPTS}} -jar ${SBT_LAUNCHER_PATH} "$@"
