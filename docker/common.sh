#!/usr/bin/env bash

readonly DOCKER_BASE_IMAGE_NAME='rmem/base'
readonly DOCKER_TEXT_IMAGE_NAME='rmem/text'
readonly DOCKER_WEB_IMAGE_NAME='rmem/web'
readonly DOCKER_REG_IMAGE_NAME='rmem/regression'
readonly DOCKER_LITMUS_IMAGE_NAME='litmus-tests'

readonly DOCKER_UID=1000   # "$(id --user)"
readonly DOCKER_GID=1000   # "$(id --group)"
readonly DOCKER_USER=rems  # "$(id --user --name)"
readonly DOCKER_GROUP=rems # "$(id --group --name)"
