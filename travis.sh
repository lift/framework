#!/bin/bash

set -ev

./liftsh test

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]; then
  mkdir -p ~/.sbt/0.13/
  openssl aes-256-cbc -K $encrypted_a177bbd76133_key -iv $encrypted_a177bbd76133_iv -in .credentials.enc -out ~/.sbt/0.13/.credentials -d

  if [ "${TRAVIS_BRANCH}" = "master" ]; then
    ./liftsh publish
  elif [ "${TRAVIS_BRANCH}" = "lift_26" ]; then
    ./liftsh ++2.10.4 "project lift-framework-pre-111" publish
    ./liftsh ++2.11.1 publish
  fi

  rm ~/.sbt/0.13/.credentials
fi
