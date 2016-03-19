#!/bin/bash

RANGE=$(echo ${TRAVIS_COMMIT_RANGE:-HEAD} | sed -e 's/\.\.\./../g')
echo "RANGE: $RANGE"

CHANGES=$(git diff-tree -r $RANGE)
echo "CHANGES: $CHANGES"

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$CHANGES" == *"version.sbt"* ]]; then
    PUBLISH="publish"
else
    PUBLISH=""
fi

echo sbt clean compile test $PUBLISH
sbt clean compile test $PUBLISH
