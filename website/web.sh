#!/bin/bash

set -e

cp -R docs website/src
rm -rf website/src/docs/rfcs
rm website/src/docs/lang/*/README.md
cp -R blog website/src
cp -R images website/src
rm website/src/blog/README.md
cp PRIVACY.md website/src
cd website

langs=()

# this loop finds out the available languages
for file in langs/*.json; do
  if [ -f "$file" ]; then
    file_name=$(basename "$file")
    file_name=${file_name%.*}
    langs+=($file_name)
  fi
done

npm install
cp node_modules/lottie-web/build/player/lottie.min.js src/js
node merge_translations.js
node customize_docs_frontmatter.js

# creating folders for each language for internationalization
for lang in "${langs[@]}"; do
  mkdir -p src/$lang
  cp src/index.html src/$lang
  cp src/contact.html src/$lang
  cp src/invitation.html src/$lang
  cp src/fdroid.html src/$lang
  echo "{\"lang\":\"$lang\"}" > src/$lang/$lang.json
  echo "done $lang copying"
done

npm run build

for lang in "${langs[@]}"; do
  rm -rf src/$lang
  echo "done $lang deletion"
done

# for val in "${langs[@]}"; do
#   json_content=$(echo "$json_content" | jq ". + {$val: $(jq . langs/$val.json)}")
# done
# echo "$json_content" > translations.json


# keys of the english language are used as the base keys
# base_keys=($(jq -r 'keys[]' 'langs/en.json'))
# this program generates a combined translations.json file
# main_json_obj="{}"
# for key in "${base_keys[@]}"; do
#   val_json_obj="{}"
#   for lang in "${langs[@]}"; do
#     val="$(jq .["\"$key\""] langs/$lang.json)"
#     if [ ! -z "$val" ] && [ "$val" != "null" ]; then
#       val_json_obj=$(echo "$val_json_obj" | jq ". + {$lang: $val}")
#     fi
#   done
#   main_json_obj=$(echo "$main_json_obj" | jq ". + {\"$key\": $val_json_obj}") 
# done
# echo "$main_json_obj" > translations.json