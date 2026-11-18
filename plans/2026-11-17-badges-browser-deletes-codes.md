# Badges page: browsers that delete codes

## Decisions

- Warnings are added on the screens below.
- WebKit is recognised by its user agent: `AppleWebKit/` without `Chrome/` or `Android`.
- A `Retention` value is passed to the screens:
  - the code screen and the return-to-app screen, in place of `savedLocally`
  - the paid-no-code screen
  - Your codes, in place of `keepsNewCodes`

```
notKept         a value cannot be written to localStorage at page load, or is not read back
kept            a value written to localStorage at page load is read back, and browserDeletesIdle is false
keptUntilIdle   a value written to localStorage at page load is read back, and browserDeletesIdle is true
```

- On the code screens, `notKept` is also passed when this code was not written to localStorage.

- The period on the screens is 7 days, with U+00A0 in "7 days" and in "key press".
- On Your codes:
  - empty list, in every browser: "Codes stored in this browser are listed here."
  - listed orders, for `kept` and `keptUntilIdle`: "Codes are stored only in the browser they were bought in."
- "This code is not in this browser" replaces "This code is not on this device".
- "Forget everything in this browser" replaces "Forget everything on this device".
- At phone width, the Your codes list row:
  - code: `.75rem`
  - row gap: `6px`
  - row right padding: `4px`
  - Copy control padding: `0 8px`
- Line breaks:
  - `h1.tight`: `text-wrap: balance`
  - notice, warning and info lines: `text-wrap: pretty`
  - muted lines directly inside a panel (`.panel > p.muted`): `text-wrap: pretty`
  - the empty-list lede, and the listed-orders lede for `kept` and `keptUntilIdle`: two `.half` spans

## Where the warning is shown

The period text: "after 7 days of browser use without a tap, click or key press on this site."

| Screen | Box | Title | Lines |
|---|---|---|---|
| Paid, code shown | warning under "This is the only copy." | This browser can delete the code | It may do so after the period. Redeem the code now, or keep a copy outside this browser. |
| Paid, opened from the app, code hidden | warning above "Show code" | This browser can delete the code | It may do so after the period. If the app does not add the badge, show the code and keep a copy outside this browser. |
| Your codes, with a paid code | warning above the list | This browser can delete codes | It may do so after the period. Copy each code you have not redeemed and keep it outside this browser. |
| Your codes, without a paid code | grey line under the lede | | This browser can delete codes after the period. |
| Your codes, empty | grey line under the lede | | This browser can delete codes after the period. |
| Paid order opened without its code | notice, between its title and "Quote the reference below and we will sort it out." | unchanged | If the code was bought in this browser, it may have been deleted after the period. |

```mermaid
flowchart TD
  load[page load] --> w0{value is written to localStorage and read back}
  w0 -->|no| nk[notKept]
  w0 -->|yes| d{browserDeletesIdle of navigator.userAgent}
  d -->|false| k[kept]
  d -->|true| u[keptUntilIdle]
  nk --> other[Your codes, paid-no-code: the page-load value]
  k --> other
  u --> other
  nk --> csn[code screens: notKept]
  k --> cw{this code written to localStorage}
  u --> cw
  cw -->|no| csn
  cw -->|yes| same[code screens: the page-load value]
```

| Screen | `notKept` | `kept` | `keptUntilIdle` |
|---|---|---|---|
| Code screen | "This code could not be saved in this browser." box | only-copy box | only-copy box, then the "This browser can delete the code" box |
| Return-to-app, code hidden | code shown at once, as on the code screen | no box | "This browser can delete the code" box above "Show code" |
| Return-to-app, code shown | | only-copy box | only-copy box, then the "This browser can delete the code" box |
| Your codes, with a paid code | "This browser cannot save anything new right now." lede | "Codes are stored only…" lede | "Codes are stored only…" lede, then the "This browser can delete codes" box |
| Your codes, without a paid code | "This browser cannot save anything new right now." lede | "Codes are stored only…" lede | "Codes are stored only…" lede, then the grey line |
| Your codes, empty | lede | lede | lede, then the grey line |
| Paid-no-code | notice | notice | notice with the deletion line |

## Tests

- `test/store.test.ts`, user agents of WebKit:
  - Safari on macOS and on iPad in desktop mode
  - Safari on iPhone
  - Chrome on iPhone
  - Firefox on iPhone
  - Edge on iPhone
  - an in-app web view on iPhone
  - GNOME Web on Linux
- `test/store.test.ts`, other user agents:
  - Chrome on Windows
  - Chrome on macOS
  - Chromium on Linux
  - Edge on Windows
  - Chrome on Android
  - an Android web view
  - Samsung Internet
  - Firefox on Windows
  - Firefox on Android
  - the Android stock browser
  - an empty user agent
- `test/screens.test.ts`, with and without the deletion warning, and the title and lines of each warning box:
  - the code screen
  - the return-to-app screen before and after Show code
  - Your codes
  - the paid-no-code screen
- `test/main-safari-app.test.ts`: the paid ending opened from the app, and Your codes, in Safari on iPhone.
- `test/main-safari-code.test.ts`: the code screen in Safari on macOS.
- `test/main-chrome-code.test.ts`: the code screen and Your codes in Chrome on macOS, without the warning.
- `test/main-safari-lost.test.ts`: a paid order opened without its code in Safari on macOS.
- `test/main-safari-no-storage.test.ts`: Your codes in Safari on macOS with localStorage refused, without the warning.
