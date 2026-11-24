#!/bin/bash

set -euo pipefail

npm install --no-audit --no-fund "$@" && exit
echo "npm install failed, retrying in 7 minutes" >&2
sleep 420
npm install --no-audit --no-fund "$@"
