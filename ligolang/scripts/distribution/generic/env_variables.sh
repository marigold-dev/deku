# This file is a substitute for env variables configured in the CI
# in case you want to run the "CI scripts" on your own
# You can load the following variables using:
# source ./scripts/distribution/generic/env_variables.sh

export LIGO_REGISTRY_IMAGE_BASE_NAME="ligolang/ligo"
# packages build locally are tagget by the 'short' commit hash,
# instead of the build/job/pipeline ID as in the CI to avoid possible confusion
# ligo_incrementing-id_commit-hash
export CI_JOB_ID="0"
export CI_COMMIT_SHORT_SHA="$(git rev-parse --short HEAD)"
export CI_COMMIT_SHA="$(git rev-parse HEAD)"
export COMMIT_DATE="$(git show --no-patch --format=%ci)"
export LIGO_VERSION="Rolling release\
Commit SHA: $CI_COMMIT_SHA\
Commit Date: $COMMIT_DATE"
export LIGO_DIST_DIR="./dist"
