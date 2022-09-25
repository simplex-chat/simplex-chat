#!/bin/bash

cp -R blog website/src
cp -R images website/src
rm website/src/blog/README.md
cd website
npm install
npm run build
