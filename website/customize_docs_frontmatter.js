const fs = require('fs');
const path = require('path');
const matter = require('gray-matter');

function docLanguage(relativePath) {
    const [folder, language] = relativePath.split(path.sep);
    return folder === 'lang' ? language : 'en';
}

function customizeDocs(sourceDir, destDir, relativePaths) {
    const docs = relativePaths.map((relativePath) => {
        const parsedMatter = matter(fs.readFileSync(path.join(sourceDir, relativePath), 'utf-8'));
        return {
            relativePath,
            fileName: path.basename(relativePath, '.md'),
            language: docLanguage(relativePath),
            content: parsedMatter.content,
            data: { ...parsedMatter.data },
        };
    });

    const languagesByFileName = new Map();
    docs.forEach(({ fileName, language }) => {
        if (!languagesByFileName.has(fileName)) languagesByFileName.set(fileName, []);
        languagesByFileName.get(fileName).push(language);
    });

    const enRevisions = new Map(docs
        .filter((doc) => doc.language === 'en')
        .map((doc) => [doc.relativePath, doc.data.revision]));

    docs.forEach(({ relativePath, fileName, language, content, data }) => {
        const permalink = `/docs/${relativePath.replace(/\.md$/, '.html')}`.toLowerCase();

        if (fileName === 'JOIN_TEAM') {
            data.active_jobs = true;
        }
        if (!data.permalink) data.permalink = permalink;

        data.supportedLangsForDoc = languagesByFileName.get(fileName);

        if (!data.layout) data.layout = 'layouts/doc.html';

        if (language === 'en') {
            data.version = 'new';
        } else {
            const enRelativePath = path.join(...relativePath.split(path.sep).slice(2));
            if (enRevisions.has(enRelativePath)) {
                const isOld = new Date(data.revision) < new Date(enRevisions.get(enRelativePath));
                data.version = isOld ? 'old' : 'new';
            }
        }

        const destPath = path.join(destDir, relativePath);
        const updatedFileContent = matter.stringify(content, data);
        if (!fs.existsSync(destPath) || fs.readFileSync(destPath, 'utf-8') !== updatedFileContent) {
            fs.mkdirSync(path.dirname(destPath), { recursive: true });
            fs.writeFileSync(destPath, updatedFileContent, 'utf-8');
        }
    });
}

module.exports = customizeDocs;
