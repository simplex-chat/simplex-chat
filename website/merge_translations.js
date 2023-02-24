const fs = require("fs");
const path = require("path");

let supportedLangs = []
const translationsDirectoryPath = path.resolve(__dirname, "langs")

const files = fs.readdirSync(translationsDirectoryPath);
const jsonFileNames = files.filter(file => file.endsWith('.json'));
supportedLangs = jsonFileNames.map(file => file.replace('.json', ''))

// keys of the english language are used as the base keys
const enStrings = require("./langs/en.json")

const languages = {}
for (const lang of supportedLangs) {
    languages[lang] = require(`./langs/${lang}.json`)
}

// this program generates a combined translations.json file
const translations = {}
for (const key in enStrings) {
    const langStrings = {}
    for (const lang of supportedLangs) {
        const str = languages[lang][key]
        if (str) langStrings[lang] = str
    }
    translations[key] = langStrings
}

saveFile("translations.json", translations)
// the list in the supported_languages.json file is used as the reference list for displaying available languages on the frontend
saveFile("src/_data/supported_languages.json", {"langs": supportedLangs})

function saveFile(relPath, data) {
    filePath = path.resolve(__dirname, relPath)
    fs.writeFileSync(filePath, JSON.stringify(data, undefined, "  "), 'utf-8');
    console.log(`Data was successfully written to ${filePath}`);
}
