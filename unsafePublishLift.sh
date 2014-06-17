#!/bin/bash
#It is called unsafe so most people stay away from this
#But this script is safe to use if you are trying to publish Lift Framework to sonatype
#as a release version (including Milestones and RC's


## This scripts runs on mac's bash terminal

# Exit on any errors and on unbound vars to be safe
set -o errexit
set -o nounset


BUILDLOG=/tmp/Lift-do-release-`date "+%Y%m%d-%H%M%S"`.log

# This script is an attempt to automate the Lift release process
#
# From Indrajit, the steps on each module (superbuild, framework, examples) are:
#
# 1. git checkout -b <version>
# 2. ./liftsh 'set project.version <version>'
# 3. Edit project/plugins/Plugins.scala to change the version of lift-sbt
# 4. git commit -v -a -m "Prepare for <version>"
# 5. git push origin <version>
# 6. git tag <version>-release
# 7. git push origin <version>-release
# 8. LIFTSH_OPTS="-Dpublish.remote=true -Dsbt.log.noformat=true" ./liftsh clean-cache clean-plugins reload +clean-lib +update +clean +publish-signed
# 9. Wait for happiness

SCRIPTVERSION=0.1

##### Utility functions (break these out into an include?) #####
# Basically yes/no confirmation with customized messages
# Usage: confirm "prompt"
# Returns 0 for yes, 1 for no
function confirm {
    while read -p "$1 [yes/no] " CONFIRM; do
        case "`echo $CONFIRM | tr [:upper:] [:lower:]`" in
            yes)
                return 0
                ;;
            no)
                return 1
                ;;
            *)
                echo "Please enter yes or no"
                ;;
        esac
    done
}

function debug {
    #echo $@
    echo -n ""
}

function die {
    echo $@
    exit 1
}

# Locate our base directory (taken from http://blog.eitchnet.ch/?p=242)
SCRIPT_NAME="${PWD##*/}"
SCRIPT_DIR="${PWD%/*}"

# if the script was started from the base directory, then the
# expansion returns a period
if test "$SCRIPT_DIR" == "." ; then
  SCRIPT_DIR="$PWD"
# if the script was not called with an absolute path, then we need to add the
# current working directory to the relative path of the script
elif test "${SCRIPT_DIR:0:1}" != "/" ; then
  SCRIPT_DIR="$PWD/$SCRIPT_DIR"
fi

echo -e "\n*********************************************************************"
echo -e "SCRIPT_DIR is ${SCRIPT_DIR}"
echo -e "\n*********************************************************************"

##### End Utility Functions #####


echo -e "\n*********************************************************************"
printf    "* Lift Full Release build script version %-26s *\n" "$SCRIPTVERSION"
#echo      "* Default choices for prompts are marked in capitals                *"
printf    "*********************************************************************\n\n"

echo -e "Build output logged to $BUILDLOG\n"


# CouchDB will blow up with HTTP proxy set because it doesn't correctly interpret the return codes
set +o nounset
if [ ! -z "${http_proxy}" -o ! -z "${HTTP_PROXY}" ]; then
    echo -e "CouchDB tests will fail with http_proxy set! Please unset and re-run.\n"
    exit
fi
set -o nounset

# First, let's confirm that we really want to release...
confirm "Are you certain you want a release build?" || die "Cancelling release build."

echo -e "\nProceeding...\n"

# Now we need to know what version we're releasing
read -p "Please enter the version of the release: " RELEASE_VERSION

# Sanity check on the release version
if ! echo $RELEASE_VERSION | egrep -x '[0-9]+\.[0-9]+(-(M|RC)[0-9]+)?' > /dev/null; then
    confirm "$RELEASE_VERSION does not appear to be a valid version. Are you sure?" ||
      die "Cencelling release build!"
fi

# Perform a sanity check on the modules first
for MODULE in framework ; do
    cd ${SCRIPT_DIR}/${MODULE}

    echo "We cd'ed into `pwd`"

    # ensure that we're on master, and that we're up-to-date
    CURRENT_BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    debug "Current branch for $MODULE is $CURRENT_BRANCH"

    if [ "${CURRENT_BRANCH}" != "master" ]; then
        echo "Currently releases can only be built from master. $MODULE is on branch $CURRENT_BRANCH. Aborting build."
        exit
    fi

    # ensure that we don't have any outstanding changes
    if git status | grep -q "Changes not staged for commit" ; then
        die "There are outstanding changes in $MODULE. Aborting build."
    else
        echo "All changes are committed, moving on"
    fi

done

echo -e "\nPre-build tests passed. Initiating release build of LiftWeb version $RELEASE_VERSION\n"

# For the remaining modules, we follow indrajit's steps outlined above
for MODULE in framework ; do
    echo -e "\nStarting build on $MODULE module"
    cd ${SCRIPT_DIR}/${MODULE} || die "Could not change to $MODULE directory!"

    git checkout -b ${RELEASE_VERSION} >> ${BUILDLOG} || die "Error creating work branch!"


    ./liftsh ";set version in ThisBuild :=  \"${RELEASE_VERSION}\" ; session save  " >> ${BUILDLOG} || die "Could not update project version in SBT!"

    git commit -v -a -m "Prepare for ${RELEASE_VERSION}" >> ${BUILDLOG} || die "Could not commit project version change!"

#git push origin ${RELEASE_VERSION} >> ${BUILDLOG} || die "Could not push project version change!"

    git tag ${RELEASE_VERSION}-release >> ${BUILDLOG} || die "Could not tag release!"

#git push origin ${RELEASE_VERSION}-release >> ${BUILDLOG} || die "Could not push release tag!"

    # Do a separate build for each configured Scala version so we don't blow the heap
    for SCALA_VERSION in $(grep crossScalaVersions build.sbt | cut -d '(' -f 2 |  sed s/[,\)\"]//g ); do
        echo -n "  Building against Scala ${SCALA_VERSION}..."
        if ! ./liftsh ++${SCALA_VERSION} clean update test publishSigned >> ${BUILDLOG} ; then
            echo "failed! See build log for details"
            exit
        fi
        echo "complete"
    done

    echo "Build complete for module ${MODULE}"

done

echo -e "\n\nRelease complete!"
echo -e "\n\nPlease update the lift_sbt_2.6 templates!"
echo -e "\n\nand write something about this release on the liftweb.net site."



