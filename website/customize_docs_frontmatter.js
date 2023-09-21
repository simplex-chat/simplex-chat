const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

const directoryPath = path.resolve(__dirname, 'src/docs');
const langFolder = 'lang';
const enFiles = {};

function traverseDirectory(directory, currentLanguage = 'en', result = {}, callback) {
    const filesAndDirectories = fs.readdirSync(directory);

    filesAndDirectories.forEach((fileOrDirectoryName) => {
        const fullPath = path.join(directory, fileOrDirectoryName);

        if (fs.statSync(fullPath).isDirectory()) {
            // If the subdirectory is inside the 'lang' folder, update the current language
            if (directory.endsWith('/lang')) {
                currentLanguage = fileOrDirectoryName;
            }

            // Recursively traverse the subdirectories
            traverseDirectory(fullPath, currentLanguage, result, callback);
        } else {
            // Process the file only if it has the '.md' extension
            if (path.extname(fullPath) === '.md') {
                // Add the language to the file's language array or create a new array if it doesn't exist
                const fileName = path.basename(fullPath, '.md');
                if (!result[fileName]) {
                    result[fileName] = [];
                }
                result[fileName].push(currentLanguage);
            }
            if (callback) {
                callback(fullPath, currentLanguage);
            }
        }
    });

    return result;
}

const fileLanguageMapping = traverseDirectory(directoryPath);

// Update the frontmatter of each Markdown file
Object.entries(fileLanguageMapping).forEach(([fileName, languages]) => {
    // Find and update the frontmatter of each Markdown file
    traverseDirectory(directoryPath, null, {}, (fullPath, currentLanguage) => {
        if (path.basename(fullPath) === `${fileName}.md`) {
            // Read the existing frontmatter
            const fileContent = fs.readFileSync(fullPath, 'utf-8');
            const parsedMatter = matter(fileContent);
            const relativePath = path.relative(directoryPath, fullPath);

            // Calculate the permalink based on the file's location
            const linkPath = path.relative(directoryPath, fullPath).replace(/\.md$/, '.html');
            const permalink = `/docs/${linkPath}`.toLowerCase();

            if (fileName === 'JOIN_TEAM') {
                parsedMatter.data.active_jobs = true;
            }
            if (!parsedMatter.data.permalink) parsedMatter.data.permalink = permalink;

            // Update the frontmatter with the new languages list
            parsedMatter.data.supportedLangsForDoc = languages;

            // Add the layout value
            if (!parsedMatter.data.layout) parsedMatter.data.layout = 'layouts/doc.html';

            if (fullPath.startsWith(path.join(directoryPath, langFolder))) {
                // Non-English files
                const [language, ...rest] = relativePath.split(path.sep).slice(1);
                const enFilePath = path.join(directoryPath, ...rest);

                if (enFiles[enFilePath]) {
                    const enRevision = new Date(enFiles[enFilePath].revision);
                    const currentRevision = new Date(parsedMatter.data.revision);

                    const isOld = currentRevision < enRevision;
                    // Add the version value
                    parsedMatter.data.version = isOld ? 'old' : 'new';
                }
            } else {
                // English files
                enFiles[fullPath] = { revision: parsedMatter.data.revision };
                // Add the version value
                parsedMatter.data.version = 'new';
            }

            // Save the updated frontmatter and content back to the file
            const updatedFileContent = matter.stringify(parsedMatter.content, parsedMatter.data);
            fs.writeFileSync(fullPath, updatedFileContent, 'utf-8');
        }
    });
});
