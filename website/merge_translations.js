const fs = require("fs");
const path = require("path");

let supportedLangs = []
const translationsDirectoryPath = path.resolve(__dirname, "langs")

const files = fs.readdirSync(translationsDirectoryPath);
const jsonFileNames = files.filter(file => file.endsWith('.json'));
supportedLangs = jsonFileNames.map(file => file.replace('.json', ''))

// keys of the english language are used as the base keys
const enStrings = require(path.resolve(__dirname, "langs/en.json"))

const languages = {}
for (const lang of supportedLangs) {
    languages[lang] = require(path.resolve(__dirname, `langs/${lang}.json`))
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

saveFile(path.resolve(__dirname, "translations.json"), translations)

function saveFile(relPath, data) {
    filePath = path.resolve(__dirname, relPath)
    fs.writeFileSync(filePath, JSON.stringify(data, undefined, "  "), 'utf-8');
    console.log(`Data was successfully written to ${filePath}`);
}
