#!/bin/bash

set -e

sbt ++$TRAVIS_SCALA_VERSION test

./npmsh

if [ "${TRAVIS_PULL_REQUEST}" = "false" ] && [ -z "$DISABLE_PUBLISH" ]; then
  mkdir -p ~/.sbt/1.0/
  openssl aes-256-cbc -K $encrypted_a177bbd76133_key -iv $encrypted_a177bbd76133_iv -in .credentials.enc -out ~/.sbt/1.0/.credentials -d

  sbt ++$TRAVIS_SCALA_VERSION publish

  rm ~/.sbt/1.0/.credentials
fi
