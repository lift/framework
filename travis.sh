#!/bin/bash

set -ev

./liftsh test

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]; then
  echo "Attempting publish!"

  if [ "${TRAVIS_BRANCH}" = "master" ]; then
    sbt publish
  fi

  if [ "${TRAVIS_BRANCH}" = "lift_26" ]; then
    sbt ++2.10.4 "project lift-framework-pre-111" publish
    sbt ++2.11.1 publish
  fi
fi
