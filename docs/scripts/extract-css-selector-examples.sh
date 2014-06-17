WORK_DIR=`pwd`
HTML_DIR="$WORK_DIR"

if [[ ! -z "$DEXY_ROOT" ]]
then
  cd $DEXY_ROOT

  until [ "$(basename $HTML_DIR)" == "docs" ]
  do
    HTML_DIR=$(dirname $HTML_DIR)
  done
fi

sbt -Dsbt.log.noformat=true "project lift-documentation-helpers" \
    "run-main net.liftweb.documentation.ExtractCssSelectorExamples \"$HTML_DIR\" \"$DEXY_ROOT\""
