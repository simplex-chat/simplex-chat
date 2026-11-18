# Typo "messsages" in the member reports preference texts

## Problem

The member reports preference shows "messsages" instead of "messages" in three English texts, on Android, desktop and iOS:

- "Allow to report messsages to moderators."
- "Members can report messsages to moderators."
- "Subscribers can report messsages to moderators."

## Cause

The texts were added with the typo in #5620, and the typo was copied into the channel variant in #6889.

- Android and desktop: `enable_sending_member_reports`, `group_members_can_send_reports` and `group_members_can_send_reports_channel` in the base `strings.xml`. Translations are keyed by name; none contains the typo.
- iOS: three literals in `ChatTypes.swift` (`GroupFeature.enableDescription`). The English text is the localization key in `Localizable.strings` and the `trans-unit` id and `source` in each `.xliff`.

## Fix

- `strings.xml` and `ChatTypes.swift`: "messsages" is corrected to "messages".
- Every `*.lproj/Localizable.strings` and `SimpleX Localizations/*.xcloc/Localized Contents/*.xliff`: keys, `trans-unit` ids and `source` elements are renamed to the corrected text. Translated values are unchanged. The `target` elements of `en.xliff` are corrected.

## Scope

Android, desktop and iOS texts and every iOS localization file that contains these keys; translated values are unchanged.
