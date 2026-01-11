#!/bin/bash

set -e

cd ..
./website/web.sh
cd website
npm run start
