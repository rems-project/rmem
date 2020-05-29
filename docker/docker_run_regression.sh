#!/usr/bin/env bash

# set -x

readonly SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "$SCRIPTS_DIR/common.sh"

if [[ "$#" != 3 || "$1" != "-j" ]] ; then
  echo "Usage: docker_run_nightly.sh -j n NAME"
  echo
  echo "NAME: name for the docker container. If container with the same name"
  echo "already exists, it will be removed."
  echo
  echo "Options:"
  echo "  -j n   Use 'n' jobs to run tests"
  exit 1
fi
readonly NJOBS="$2"
readonly NAME="$3"

# Remove old containers, if exist
[[ -z "$(docker container ls --all --quiet --filter "name=^${NAME}$")" ]] || docker rm "${NAME}"

# docker create --name temp-test "${DOCKER_REG_IMAGE_NAME}"
# docker cp ${SCRIPTS_DIR}/../../litmus-tests-regression-machinery/run_nightly.sh temp-test:/home/${DOCKER_USER}/litmus-tests-regression-machinery/
# docker commit test "${DOCKER_REG_IMAGE_NAME}-test"

# Run the tests
docker run \
  --name "${NAME}" \
  --tty \
  --workdir /home/${DOCKER_USER}/litmus-tests-regression-machinery \
  "${DOCKER_REG_IMAGE_NAME}" \
  ./run_nightly.sh -j "$NJOBS"

# Copy the results out of the container
rm -rf results
docker cp "${NAME}:/home/${DOCKER_USER}/litmus-tests-regression-machinery/nightly-results" results
