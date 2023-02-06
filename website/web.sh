#!/bin/bash

cp -R blog website/src
cp -R images website/src
rm website/src/blog/README.md
cd website

sudo apt update && sudo apt install -y jq
translations_file=$(cat translations.json)
# extract the top-level keys and convert them into an array
langs=($(echo "$translations_file" | jq -r 'keys[]'))

# creating folders for each language for internationalization
for lang in "${langs[@]}"; do
  mkdir src/$lang
  cp src/index.html src/$lang
  cp src/contact.html src/$lang
  cp src/invitation.html src/$lang
  cp src/blog.html src/$lang
  mkdir src/$lang/blog
  cp -R src/blog/images src/$lang/blog

  echo "done $lang copying"
done

npm install
npm run build

for lang in "${langs[@]}"; do
  rm -rf src/$lang
  echo "done $lang deletion"
done
