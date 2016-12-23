#!/bin/bash

set -ev

sbt ++$TRAVIS_SCALA_VERSION test

./npmsh

if [ "${TRAVIS_PULL_REQUEST}" = "false" ]; then
  mkdir -p ~/.sbt/0.13/
  openssl aes-256-cbc -K $encrypted_a177bbd76133_key -iv $encrypted_a177bbd76133_iv -in .credentials.enc -out ~/.sbt/0.13/.credentials -d

  # Include the no-java-comments work-around so that publishing for Scala 2.12 will work correctly.
  # See: https://github.com/scala/scala-dev/issues/249
  sbt ++$TRAVIS_SCALA_VERSION "set scalacOptions in (Compile, doc) += \"-no-java-comments\"" publish

  rm ~/.sbt/0.13/.credentials
fi
