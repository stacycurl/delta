#!/bin/bash

RANGE=$(echo ${TRAVIS_COMMIT_RANGE:-HEAD} | sed -e 's/\.\.\./../g')
CHANGES=$(git diff-tree -r $RANGE)

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$CHANGES" == *"version.sbt"* ]]; then
    PUBLISH="publish"
else
    PUBLISH=""
fi

echo sbt clean compile test $PUBLISH
sbt clean compile test $PUBLISH
