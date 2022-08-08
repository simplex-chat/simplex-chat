#!/bin/bash

cp -R blog website/src
rm website/src/blog/README.md
cp -R website/src/blog/images website/src/img
rm -rf website/src/blog/images
cp -R images/* website/src/img/images

for f in website/src/blog/*.md
do
    sed -i 's@\.\./images@/img/images@g' $f
    sed -i 's@\./images@/img/images@g' $f
    # sed -i 's@(\./@(/blog/@g' $f
    # sed -i 's@\.md@/@g' $f
	echo "$f"
done
cd website
npm install
npm run build
npm run build-tailwind