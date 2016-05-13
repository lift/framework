WORK_DIR=`pwd`

if [[ ! -z "$DEXY_ROOT" ]]
then
  cd $DEXY_ROOT
fi

sbt -Dsbt.log.noformat=true -Dspecs2.color=false "project lift-documentation-helpers" "test"
