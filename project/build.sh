#!/bin/bash

CHANGES=$(git diff-tree --no-commit-id --name-only -r ${TRAVIS_COMMIT_RANGE:-HEAD})

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$CHANGES" == *"version.sbt"* ]]; then
    PUBLISH="publish"
else
    PUBLISH=""
fi

echo sbt clean compile test $PUBLISH
sbt clean compile test $PUBLISH