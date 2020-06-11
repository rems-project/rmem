#!/usr/bin/env bash

set -o errexit
set -o nounset
#set -o xtrace
shopt -s extglob

readonly SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "$SCRIPTS_DIR/common.sh"

usage() {
  echo "Usage: docker_build.sh base|text|dev-base"
  echo "       docker_build.sh dev-text -d <path/to/rmem> [--force]"
  echo "       docker_build.sh litmus|regression|web|all --ssh-key <file>"
  echo "       docker_build.sh dev -d <path/to/rmem> --ssh-key <file>"
  echo
  echo "Options:"
  echo "  --ssh-key <file>   Use <file> to access the private litmus-test"
  echo "                     repositories on github"
  echo "  -d <path>          Use <path> as the path to a local rmem installation"
  echo "                     to be used in docker"
  echo "  --force            Do not check <path/to/rmem> contains an installation of rmem"
}

if [[ "$#" -lt 1 ]] ; then
  usage
  exit 1
fi

case "$1" in
  litmus|regression|web|all)
    readonly TARGET="$1"
    shift 1
    if [[ "$#" -lt 2 || "$1" != "--ssh-key" ]] ; then
      usage
      exit 1
    fi
    readonly SSH_KEY="$2"
    shift 2
    ;;
  dev-text)
    readonly TARGET="$1"
    shift 1
    if [[ "$#" -lt 2 || "$1" != "-d" ]]; then
      usage
      exit 1
    fi
    readonly RMEM_DIR="$2"
    shift 2
    if [[ "$#" -lt 1 || "$1" != "--force" ]]; then
      readonly RMEM_FORCE=0
    elif [[ "$#" -lt 1 ]]; then
      usage
      exit 1
    else
      readonly RMEM_FORCE=1
    fi
    ;;
  dev)
    readonly TARGET="$1"
    shift 1
    if [[ "$#" -lt 4 || "$1" != "-d" || "$3" != "--ssh-key" ]]; then
      usage
      exit 1
    fi
    readonly RMEM_FORCE=0
    readonly RMEM_DIR="$2"
    readonly SSH_KEY="$4"
    shift 4
    ;;
  base|text|dev-base)
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

build_dev_base() {
  docker build \
    --tag "$DOCKER_LOCAL_BASE_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    - < "$SCRIPTS_DIR/Dockerfile.dev"
}

build_dev_text() {
  # heuristc to check that this dir is really rmem
  # if not give the user an opt-in way to force it.
  if [[ $RMEM_FORCE != 1 ]] && ! grep ${RMEM_DIR}/opam -q -e 'name: "rmem"' &>/dev/null ; then
    echo "'${RMEM_DIR}' does not appear to be a directory containing rmem."
    echo "Run  docker_build.sh dev-text -d '${RMEM_DIR}' --force  to force using this directory"
    echo
    usage
    exit 1
  fi

  docker build \
    --tag "$DOCKER_LOCAL_TEXT_IMAGE_NAME" \
    --build-arg GUEST_UID="$DOCKER_UID" \
    --build-arg GUEST_GID="$DOCKER_GID" \
    --build-arg GUEST_USER="$DOCKER_USER" \
    --build-arg GUEST_GROUP="$DOCKER_GROUP" \
    --build-arg MODE=opt \
    --build-arg ISA=PPCGEN,AArch64,RISCV \
    -f- ${RMEM_DIR} < "$SCRIPTS_DIR/Dockerfile.dev.text"
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
  dev-base)
    build_dev_base
    ;;
  dev-text)
    build_dev_text
    ;;
  dev)
    build_dev_base
    build_litmus
    build_dev_text
    ;;
  all)
    build_base
    build_text
    build_litmus
    build_regression
    build_web
#    build_dev  # do not build dev unless explicit
    ;;
  *)
    echo "Unexpected build target!"
    exit 1
    ;;
esac
