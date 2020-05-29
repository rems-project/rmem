#!/usr/bin/env bash

readonly SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

source "$SCRIPTS_DIR/common.sh"

if [[ "$#" != 2 || "$1" != "--port" ]] ; then
  echo "Usage: docker_run_web.sh --port PORT"
  exit 1
fi

readonly PORT="$2"

echo "Point your browser to http://127.0.0.1:$PORT/index.html"

docker run \
  --rm \
  --interactive \
  --tty \
  --network host \
  --workdir /home/${DOCKER_USER}/public_html/rmem/ \
  ${DOCKER_WEB_IMAGE_NAME} \
  python3 /home/${DOCKER_USER}/rmem/scripts/serve.py "$PORT"
