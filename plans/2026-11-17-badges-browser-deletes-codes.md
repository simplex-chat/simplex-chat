# Badges page: browsers that delete codes

## Problem

The badges page keeps a bought code only in the browser's localStorage. WebKit, the engine of Safari and of other browsers on iPhone and iPad, deletes a site's script-written storage after a number of days of browser use without a tap, click or key press on that site. A buyer who opened the page from the app later found no code on Your codes, which read "Nothing bought on this device".

| Browser | Stored codes are deleted when |
|---|---|
| Safari on macOS, iOS, iPadOS | 7 days of browser use without a tap or click on the site, when a page of a classified tracker linked or redirected to it with query parameters that Safari did not filter; 30 days otherwise, since WebKit 274398@main (February 2024), 7 days for every site before; only while tracking prevention is on |
| Other browsers on iOS and iPadOS | as Safari, while the app's tracking prevention is on: WebKit enables it in all WKWebView applications since iOS 14 |
| Chromium, Chrome, Edge | the disk is short of space, least recently used sites first; private window closed; data cleared |
| Firefox | the disk is short of space, least recently used sites first; private window closed; data cleared |

Sources:

- WebKit `ResourceLoadStatisticsStore.cpp`: `operatingDatesWindowShort { 7 }`, `operatingDatesWindowLong { 30 }`, `shouldRemoveAllButCookiesFor`, `logCrossSiteLoadWithLinkDecoration`
- https://webkit.org/tracking-prevention/
- https://webkit.org/blog/10882/app-bound-domains/
- https://developer.mozilla.org/en-US/docs/Web/API/Storage_API/Storage_quotas_and_eviction_criteria

`navigator.storage.persist()` exempts a site from the deletion when disk space is short, in Chromium and Firefox.

## Decisions

- The change adds warnings only; the page requests no persistent storage.
- WebKit is recognised by its user agent: `AppleWebKit/` without `Chrome/`, `Chromium/` or `Android`.
- In those browsers the page says that the browser can delete the code after 7 days of browsing without a tap or click on the site, the earliest case.
- The empty Your codes screen reads "No codes stored in this browser" in every browser.
- The page keeps its other texts and behaviour in other browsers.

## Where the warning is shown

| Screen | Text |
|---|---|
| Paid, code shown, "This is the only copy." box | This browser can delete it after 7 days of browsing without a tap or click on this site. Redeem it now, or copy it and keep it outside this browser. |
| Paid, opened from the app, code hidden, warning box above "Show code" | Show and copy the code if the app does not add it. This browser can delete it after 7 days of browsing without a tap or click on this site. |
| Your codes, with a paid code, warning box | Copy each code you have not redeemed. This browser can delete the codes stored here after 7 days of browsing without a tap or click on this site. |
| Your codes, empty | the deletion line under "No codes stored in this browser" |
| Paid order opened without its code ("This code is not on this device") | the deletion line in the notice |

```mermaid
flowchart LR
  ua[navigator.userAgent] --> d{browserDeletesIdle}
  d -->|WebKit| w[warning on the five screens]
  d -->|Chromium, Firefox, Android| n[screens unchanged]
```

## Tests

- `test/store.test.ts`: the user agents of Safari, Chrome, Firefox and Edge on iPhone, an iPhone web view, GNOME Web, and of Chrome, Edge, Chromium, Samsung Internet, an Android web view and Firefox on desktop and Android.
- `test/screens.test.ts`: the five screens with and without the warning.
- `test/main-app-safari.test.ts`: the paid ending opened from the app, and Your codes, in Safari on iPhone.
- `test/main-safari-code.test.ts`: the code screen in Safari on macOS.
- `test/main-safari-lost.test.ts`: a paid order opened without its code in Safari on macOS.
- Manual check with Playwright WebKit 27.2, Chromium 156 and Firefox 159: the purchase flow against the mock service, with and without the app, and the order opened after its code was cleared.
