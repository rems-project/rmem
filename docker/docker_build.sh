#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace
shopt -s extglob

readonly SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "$SCRIPTS_DIR/common.sh"

usage() {
  echo "Usage: docker_build.sh base|text"
  echo "       docker_build.sh litmus|regression|web|all --ssh-key <file>"
  echo
  echo "Options:"
  echo "  --ssh-key <file>   Use <file> to access the private litmus-test"
  echo "                     repositories on github"
}

if [[ "$#" -lt 1 ]] ; then
  usage
  exit 1
fi

case "$1" in
  litmus|regression|web|all)
    readonly TARGET="$1"
    shift 1
    if [[ "$1" != "--ssh-key" || "$#" -lt 2 ]] ; then
      usage
      exit 1
    fi
    readonly SSH_KEY="$2"
    shift 2
    ;;
  base|text)
    readonly TARGET="$1"
    shift
    ;;
  *)
    usage
    exit 1
esac

build_base() {
  docker build \
    --tag "$DOCKER_BASE_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    - < "$SCRIPTS_DIR/Dockerfile.base"
}

build_text() {
  docker build \
    --tag "$DOCKER_TEXT_IMAGE_NAME" \
    --build-arg MODE=opt \
    --build-arg ISA=PPCGEN,AArch64,RISCV \
    - < "$SCRIPTS_DIR/Dockerfile.text"
}

build_web() {
  docker build \
    --tag "$DOCKER_WEB_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    - < "$SCRIPTS_DIR/Dockerfile.web"
}

build_litmus() {
  docker build \
    --tag "$DOCKER_LITMUS_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    --build-arg SSH_KEY="$(cat "${SSH_KEY}")" \
    - < "$SCRIPTS_DIR/Dockerfile.litmus"
}

build_regression() {
  docker build \
    --tag "$DOCKER_REG_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    - < "$SCRIPTS_DIR/Dockerfile.regression"
}

case "$TARGET" in
  base)
    build_base
    ;;
  text)
    build_base
    build_text
    ;;
  web)
    build_base
    build_litmus
    build_web
    ;;
  litmus)
    build_litmus
    ;;
  regression)
    build_base
    build_text
    build_regression
    ;;
  all)
    build_base
    build_text
    build_litmus
    build_regression
    build_web
    ;;
  *)
    echo "Unexpected build target!"
    exit 1
    ;;
esac
