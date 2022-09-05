#!/bin/bash

cp -R blog website/src
rm website/src/blog/README.md
cd website
npm install
npm run build
npm run build-tailwind
