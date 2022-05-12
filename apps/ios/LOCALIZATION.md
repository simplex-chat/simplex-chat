# Localization

## Creating localization keys

There are three ways XCode generates localization keys from strings:

1. Automatically, from the texts used in standard components `Text`, `Label`, `Button`, etc.

2. All strings passed to view variables and function parameters declared as `LocalizedStringKey` type. Only string constants (possibly, with interpolation) or other variables of type `LocalizedStringKey` can be passed to these parameters. See, for example, ContentView.swift.

3. All strings wrapped in `NSLocalizedString`. Please note that such strings do not support swift interpolation, instead formatted strings should be used:

```swift
String.localizedStringWithFormat(NSLocalizedString("You can now send messages to %@", comment: "notification body"), value)
```

## Adding strings to the existing localizations

1. Choose `Product -> Export Localizations...` in the menu, choose `ios` folder as the destination and `SimpleX Localizations` as the folder name, confirm to overwrite it (make sure not to save to subfolder).
2. Add `target` keys to the localizations that were added or changed.
3. Choose `Product -> Import Localizations...` for any non-English folders - that would update Localizable files.

Localizable files values can be edited directly, the changes will be included in the next export. Following the process above though guarantees that all strings are localized.

## Development

Make sure to enable the option `Show non-localized strings` in `Product -> Scheme -> Edit scheme...` menu - it will be showing all non-localized strings as all caps.

Read more about editing XLIFF and string files here: https://developer.apple.com/documentation/xcode/editing-xliff-and-strings-files
