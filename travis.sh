#!/bin/bash

set -ev

./liftsh test

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]; then
  if [ "${TRAVIS_BRANCH}" = "master" ]; then
    ./liftsh publish
  fi

  if [ "${TRAVIS_BRANCH}" = "lift_26" ]; then
    ./liftsh ++2.10.4 "project lift-framework-pre-111" publish
    ./liftsh ++2.11.1 publish
  fi
fi
