# .well-known

This website files allow opening SimpleX Chat links (1-time invitations, contact addresses and groups) directly in the app.

## Android

File `assetlinks.json` includes certificate hashes for:

- Play Store (5E:3E:DC:C2:00:FB:A8:D5:F4:88:F3:CA:4C:32:5B:05:78:C5:6A:9C:03:A1:CC:B5:92:9C:D7:5C:7E:57:E2:4D)
- APK in GitHub releases (3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85)
- F-Droid (AE:C1:95:DC:FD:46:14:BD:3A:91:EC:26:D1:D5:14:C8:75:71:C5:CC:8D:CF:48:08:3F:92:83:14:3C:A2:B9:A6)

## iOS

`apple-app-site-association` needs to be served with `Content-type: application/json; charset=utf-8` and GitHub pages do not support adding this header to files without JSON extension.

To workaround this (thanks to [StackOverflow - Serve json data from github pages](https://stackoverflow.com/questions/39199042/serve-json-data-from-github-pages)) we're creating directory named `apple-app-site-association` with `index.json` file that contains all the necessary configs.