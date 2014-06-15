WORK_DIR=`pwd`

# If we're running in dexy, take us to the project top-level.
if [[ "$WORK_DIR" == *.dexy* ]]
then
  until [ "$(basename $WORK_DIR)" == ".dexy" ]
  do
    WORK_DIR=$(dirname $WORK_DIR)
  done
fi

cd $(dirname $WORK_DIR)

sbt "project lift-documentation-helpers" \
    "run-main net.liftweb.documentation.ExtractCssSelectorExamples"
