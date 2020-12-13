#!/bin/bash

set -e

sbt "++$TRAVIS_SCALA_VERSION test"

./npmsh

if [ "${TRAVIS_PULL_REQUEST}" = "false" ] && [ -z "$DISABLE_PUBLISH" ]; then
  mkdir -p ~/.sbt/1.0/
  openssl aes-256-cbc -K $encrypted_a177bbd76133_key -iv $encrypted_a177bbd76133_iv -in .credentials.enc -out ~/.sbt/1.0/.credentials -d

  if [[ "$TRAVIS_SCALA_VERSION" =~ ^2.13 ]]; then
    # we only have certain modules available for publishing in 2.13
    sbt ++$TRAVIS_SCALA_VERSION lift-webkit/publish lift-json/publish lift-actor/publish lift-json-ext/publish lift-record/publish lift-proto/publish lift-mapper/publish lift-common/publish lift-db/publish lift-markdown/publish lift-util/publish lift-testkit/publish lift-mongodb/publish lift-mongodb-record/publish lift-json-scalaz7/publish
  else
    sbt ++$TRAVIS_SCALA_VERSION publish
  fi

  rm ~/.sbt/1.0/.credentials
fi
