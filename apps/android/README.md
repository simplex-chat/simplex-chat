# Android App Development

This readme is currently a stub and as such is in development.

Ultimately, this readme will act as a guide to contributing to the develop of the SimpleX android app.


## Gotchas

#### SHA Signature for verification for app links/deep links

In order for the SimpleX app to be automatically adopted for opening links from https://simplex.chat the SHA certificate fingerprint for the App installed on the phone must be in the hosted [assetlinks.json](https://simplex.chat/.well-known/assetlinks.json) file on simplex.chat.

The accepted fingerprints are in the `sha256_cert_fingerprints` list.

To find your SHA certificate fingerprint perform the following steps.

1. Build and install your development version of the app as usual
2. From the terminal in Android studio run `adb shell pm get-app-links chat.simplex.app`
3. Copy the signature listed in `signatures` in the result
4. Add your signature to [assetlinks.json](https://github.com/simplex-chat/website/blob/master/.well-known/assetlinks.json) in the [website repo](https://github.com/simplex-chat/website) and make a PR. On approval, wait a few minutes for the changes to propagate to the public website and then you should be able to verify SimpleX.

More information is available [here](https://developer.android.com/training/app-links/verify-site-associations#manual-verification). If there is no response when running the `pm get-app-links` command, the intents in `AndroidManifest.xml` are likely misspecified. A verification attempt can be triggered using `adb shell pm verify-app-links --re-verify chat.simplex.app`. 

Note that this is not an issue for the app store build of the app as this is signed with our app store credentials and thus there is a stable signature over users. Developers do not have general access to these credentials for development and testing.
