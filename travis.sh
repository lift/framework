#!/bin/bash

set -ev

sbt ++$TRAVIS_SCALA_VERSION test

./npmsh

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]; then
  mkdir -p ~/.sbt/0.13/
  openssl aes-256-cbc -K $encrypted_a177bbd76133_key -iv $encrypted_a177bbd76133_iv -in .credentials.enc -out ~/.sbt/0.13/.credentials -d

  sbt ++$TRAVIS_SCALA_VERSION publish

  rm ~/.sbt/0.13/.credentials
fi
