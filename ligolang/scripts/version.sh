#!/bin/bash
version_regex='[0-9]+\.[0-9]+\.[0-9]+'
if [[ "$CI_COMMIT_TAG" =~ $version_regex ]]; then
  echo $CI_COMMIT_TAG
else
  cat <<EOF
Rolling release
Commit SHA: ${CI_COMMIT_SHA}
Commit Date: ${CI_COMMIT_TIMESTAMP}
EOF
fi
