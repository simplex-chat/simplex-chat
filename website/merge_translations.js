const fs = require("fs");
const path = require("path");

let supportedLangs = []
const translationsDirectoryPath = path.resolve(__dirname, "langs")
try {
    const files = fs.readdirSync(translationsDirectoryPath);
    const jsonFileNames = files.filter(file => file.endsWith('.json'));
    supportedLangs = jsonFileNames.map(file => file.replace('.json', ''))
} catch (err) {
    console.error(`An error occurred: ${err}`);
}

// keys of the english language are used as the base keys
const jsonData = JSON.parse(fs.readFileSync(path.resolve(__dirname, "langs/en.json"), "utf-8"))
const base_keys = Object.keys(jsonData)

// this program generates a combined translations.json file
const main_json_obj = {}
for (const key of base_keys) {
    const val_json_obj = {}
    for (const lang of supportedLangs) {
        const val = JSON.parse(fs.readFileSync(path.resolve(__dirname, `langs/${lang}.json`), "utf-8"))[key]
        if (val) {
            val_json_obj[lang] = val
        }
    }
    main_json_obj[key] = val_json_obj
}

filePath = path.resolve(__dirname, "translations.json")
try {
    fs.writeFileSync(filePath, JSON.stringify(main_json_obj), 'utf-8');
    console.log(`Data was successfully written to ${filePath}`);
} catch (err) {
    console.error(`An error occurred: ${err}`);
}

filePath = path.resolve(__dirname, "src/_data/supported_languages.json")
// the list in the supported_languages.json file is used as the reference list for displaying available languages on the frontend
try {
    fs.writeFileSync(filePath, JSON.stringify({"langs": supportedLangs}), 'utf-8');
    console.log(`Data was successfully written to ${filePath}`);
} catch (err) {
    console.error(`An error occurred: ${err}`);
}
