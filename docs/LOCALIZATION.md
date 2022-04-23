# Contributing UI localizations

Thank you for contributing the translations to SimpleX Chat UI to your native language!

Once you have contacted us and we have agreed that you will contribute and maintain the UI translations please follow this process:

1. You will have to open a new PR to the branch we have prepared for you updating the translations files for your language, so we can easily see the differences. There should be no new files added.
2. For iOS:
   - update the translation file for your language in [this folder](<../apps/ios/SimpleX Localizations>), (e.g. [this file](<../apps/ios/SimpleX Localizations/nl.xcloc/Localized Contents/nl.xliff>) is for Dutch language).
   - for each \<source\> key in the file you need to add \<target\> key. There are some specialized editors for XLIFF files, but it's easy enough to edit in any code editor too.
   - Please keep any interpolations (parts of the strings that start from %) as they are, and ask any questions if it is unclear in what context the string is used - we will add the comment.
3. For Android:
   - update string resource file `strings.xml` for your language in `values-<language code>` subfolder of [this folder](../apps/android/app/src/main/res) - e.g., [this file](../apps/android/app/src/main/res/values-nl/strings.xml) has Dutch language translations. You will have to replace english strings with the translations, keeping the comments in place.
