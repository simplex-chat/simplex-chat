# Links Page Implementation Plan

## Overview

Single page at `/links` showing 300+ external publications, reviews, bots, services, and community content about SimpleX Chat. All items rendered as HTML in the DOM for SEO. Client-side JS handles pagination via `display:none` and hash-based navigation.

Content source: `docs/LINKS.md` (parsed at build time).
Images: `docs/links/images/` (copied at build time, missing images handled gracefully).

## Architecture

The parser is a Node.js module imported directly by `.eleventy.js` (like the glossary parser), not a separate build step writing to `_data/`. It reads `docs/LINKS.md` and returns a structured array that Eleventy uses as template data.

## Files to Create

### 1. `website/parse_links.js` - Markdown parser module

Exports a function that reads `src/docs/LINKS.md` (after `web.sh` copies docs into src/) and returns an array of entry objects.

Parser logic - reads line by line, entry starts at `## `:
```
## Title                    -> title
(Original Title)            -> originalTitle (optional, detected by leading paren)
Publisher                   -> publisher
Category                   -> category
Featured                   -> featured: true (optional, detected by exact match)
Preview paragraph text.     -> preview (first non-metadata, non-empty line after above)
Image: filename.jpg         -> image (strip "Image: " prefix)
Language: German            -> language (strip "Language: " prefix)
Date: Dec 2, 2022           -> date (raw string), dateSort (normalized YYYY-MM-DD)
                            -> estimated: true if "(estimated)" in date string
https://example.com/...     -> url (bare URL line)
```

Derives from category:
- `mediaType`: "video" if category contains "video"/"livestream", "audio" if contains "podcast"/"audio", "text" otherwise

Generates:
- `id`: semantic slug from title (slugify, lowercase, truncated to reasonable length)
- `imageExists`: checks if file exists in `src/link-images/`

Returns array sorted reverse-chronologically by `dateSort`.

### 2. `website/src/links.html` - Page template

Frontmatter:
```yaml
layout: layouts/main.html
title: "SimpleX Chat Links"
description: "Reviews, articles, videos, podcasts, and community content about SimpleX Chat"
permalink: /links/
templateEngineOverride: njk
active_links: true
```

Structure:
- Page heading (i18n)
- Filter bar: language dropdown, category chips, media type chips
- Items list: reuses blog card layout pattern (same Tailwind classes from blog.html - `shadow-[0px_20px_30px_rgba(0,0,0,0.12)]`, `dark:bg-[#0B2A59]`, etc.)
- Each item is an `<article>` with data attributes: `data-lang`, `data-category`, `data-media`, `data-date`, `data-featured`, and `id` attribute (semantic slug)
- Image with `loading="lazy"`, fallback to SimpleX logo if no image
- Title as link to external URL (opens in new tab)
- Anchor icon (link/chain icon) appears on hover, links to `#link=item-id` for sharing
- Publisher, category, language badge, date shown as metadata
- Preview paragraph
- Featured items get a subtle highlight (border or background tint)
- Pagination controls at bottom

### 3. `website/src/js/links.js` - Client-side pagination/filtering

On page load:
1. Collect all `<article>` elements
2. Read hash: `#page=N` or `#link=slug`
3. Apply any active filters
4. Paginate: show N items per page, hide rest with `display:none`
5. If `#link=slug`: find the item, calculate its page, show that page, scroll to item, briefly highlight it
6. If `#page=N`: show page N

Filter logic:
- Filtering by language/category/media: iterate all articles, set `display:none` on non-matching, re-paginate the visible set
- Filters update the hash

Pagination:
- Items per page: ~20
- Page controls: prev/next + page numbers
- Clicking a page link sets `#page=N` in hash
- Hash change listener re-renders

Share anchors:
- Each item has a hover-visible link icon
- Clicking it copies `#link=item-id` to clipboard / updates URL hash
- When page loads with `#link=item-id`, JS finds the item, determines which page it falls on (accounting for active filters), shows that page, scrolls to item

## Files to Modify

### 4. `website/.eleventy.js`

At the top, after glossary parsing:
```js
const parseLinks = require('./parse_links')
```

Inside `module.exports`:
- Add `links` as global data: parsed from LINKS.md
- Add passthrough copy: `ty.addPassthroughCopy("src/link-images")`
- The docs collection already globs `src/docs/**/*.md` - LINKS.md must be excluded. Options:
  - `web.sh` deletes `src/docs/LINKS.md` after parse_links reads it
  - Or add frontmatter to LINKS.md with `eleventyExcludeFromCollections: true` and `permalink: false`
  - Simplest: delete in web.sh after copy

### 5. `website/web.sh`

After `cp -R docs website/src`:
```bash
cp -R docs/links/images website/src/link-images
```

After `node customize_docs_frontmatter.js`:
```bash
rm website/src/docs/LINKS.md   # prevent Eleventy from processing as doc page
```

### 6. `website/src/_includes/navbar.html`

Add "Links" nav item after Blog (line ~115):
```html
<li class="nav-link {% if active_links %}active{% endif %}">
    <a href="/links">
        <span class="nav-link-text">{{ "links" | i18n({}, lang) | safe }}</span>
    </a>
</li>
```

Language dropdown stays enabled on the links page - content spans 30 languages, so visitors from any language should be able to navigate and use the filters naturally.

### 7. `website/langs/en.json` (and other lang files)

Add i18n keys for page chrome:
- `"links"` - nav label
- `"links-title"` - page heading (e.g. "Community Links" or "Links to Community Publications")
- `"links-filter-language"` - "Language" dropdown label
- `"links-filter-all"` - "All" filter chip
- `"links-filter-category"` - "Category" label
- `"links-filter-media"` - "Media" label
- `"links-featured"` - "Featured" badge text

Translate these keys across all language files (same approach as the 14-key translation done earlier in this branch).

### 8. `website/src/index.html` - Homepage hero (follow-up)

Change the 5 publication logo links (`publications-btns` section, lines 135-151) from external URLs to `/links#link=semantic-slug`. The JS on the links page handles showing the correct page and scrolling to the item.

## Not in scope

- RSS feed for links
- Per-language page copies in web.sh (one page, i18n handles chrome translation via language dropdown)

## Build flow

```
web.sh:
  cp -R docs website/src              # copies LINKS.md into src/docs/
  cp -R docs/links/images website/src/link-images
  cd website
  npm install
  node merge_translations.js
  node customize_docs_frontmatter.js
  rm src/docs/LINKS.md                # prevent doc collection processing
  npm run build                        # .eleventy.js imports parse_links.js,
                                       # reads src/docs/LINKS.md -> data,
                                       # links.html renders from that data
```

Wait - there's a sequencing issue. If web.sh deletes LINKS.md before Eleventy runs, parse_links.js can't read it. Two options:
1. parse_links.js reads from `../docs/LINKS.md` (the original, not the copy)
2. web.sh deletes LINKS.md AFTER parse_links reads it but BEFORE Eleventy processes docs

Option 1 is simpler - parse_links.js always reads from repo root `docs/LINKS.md`, not from `src/docs/`. Then web.sh just never copies it (or deletes it after `cp -R docs website/src`).

Revised flow:
```
web.sh:
  cp -R docs website/src
  rm website/src/docs/LINKS.md         # immediately remove from src/docs/
  cp -R docs/links/images website/src/link-images
  cd website
  ...existing steps...
  npm run build                         # parse_links.js reads ../docs/LINKS.md
```
