#!/bin/bash

RANGE=$(echo ${TRAVIS_COMMIT_RANGE:-HEAD} | sed -e 's/\.\.\./../g')
echo "RANGE: $RANGE"

CHANGES=$(git diff-tree -r $RANGE)
echo "CHANGES: $CHANGES"

if [[ "$PGP_PASSPHRASE" != "" && "$CHANGES" == *"version.sbt"* ]]; then
  CLEAN="clean sonatypeBundleClean"
  PUBLISH="+ci-publish"
else
  CLEAN="clean"
  PUBLISH=""
fi

echo sbt $CLEAN compile test $PUBLISH
sbt $CLEAN compile test $PUBLISH
