#!/bin/bash

cp -R docs website/src
cp "README.md" "website/src/docs"

base_directory="website/src/docs"
old_file_name="README.md"
new_file_name="index.md"

# Find and rename all occurrences of README.md to index.md in the base directory and its subfolders
find "$base_directory" -type f -name "$old_file_name" -exec sh -c '
  old_path="{}";
  new_path=$(dirname "$old_path")/"'"$new_file_name"'";
  mv "$old_path" "$new_path";
  sed -i "/^| .* |$/d" "$new_path";
' \;

cp -R blog website/src
cp -R images website/src
rm website/src/blog/README.md
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

node merge_translations.js

# creating folders for each language for internationalization
for lang in "${langs[@]}"; do
  mkdir src/$lang
  cp src/index.html src/$lang
  cp src/contact.html src/$lang
  cp src/invitation.html src/$lang
  echo "{\"lang\":\"$lang\"}" > src/$lang/$lang.json
  echo "done $lang copying"
done

npm install
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