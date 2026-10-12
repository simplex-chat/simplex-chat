#!/bin/bash

set -e

cd ..
./website/web.sh
cd website
node copy_sources.js --watch &
trap "kill $! 2>/dev/null || true" EXIT
npm run start
