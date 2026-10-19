Revision 1, 2026-10-19, draft

Evgeny Poberezkin

# Publishing SimpleX addresses on websites

## Table of contents

- [Abstract](#abstract)
- [Conventions](#conventions)
- [Terms](#terms)
- [Link relation types](#link-relation-types)
  - [simplex-channel](#simplex-channel)
  - [simplex-contact](#simplex-contact)
  - [Target attributes](#target-attributes)
  - [HTML](#html)
  - [HTTP](#http)
- [Well-known URI](#well-known-uri)
  - [Retrieval](#retrieval)
  - [Format](#format)
  - [Example](#example)
- [Processing](#processing)
- [Security considerations](#security-considerations)
- [Privacy considerations](#privacy-considerations)
- [IANA considerations](#iana-considerations)
  - [Link relation types registration](#link-relation-types-registration)
  - [Well-known URI registration](#well-known-uri-registration)
- [HTML link type extensions](#html-link-type-extensions)
- [References](#references)

## Abstract

This document defines how a website publishes the SimpleX channels and contact addresses of its owner:

- the link relation types `simplex-channel` and `simplex-contact`, for the HTML elements `link`, `a` and `area`, and for the HTTP `Link` header field;
- the well-known URI `simplex.json`: a JSON document with the owner's channels and contact addresses, their audience, the update frequency of each channel and the protocols each contact address serves.

People and software agents subscribe to a channel to receive updates, and connect to a contact address to send requests. [Agent protocols over SimpleX](./agent-protocols.md) defines the requests an agent sends to a contact address, and an `llms.txt` section with the same addresses.

## Conventions

The key words "MUST", "MUST NOT", "SHOULD", "SHOULD NOT" and "MAY" in this document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when, and only when, they appear in all capitals.

JSON schemas use JSON Type Definition [RFC8927].

## Terms

- **SimpleX link**: a connection link of the SimpleX Agent protocol, in the full or the short syntax, with the `https` or the `simplex` scheme [AGENT].
- **Channel link**: a short SimpleX link of type `c`, for example `https://smp12.simplex.im/c#<key>`.
- **Contact address link**: a short SimpleX link of type `a`, for example `https://smp12.simplex.im/a#<key>`, or a full SimpleX link of type `contact`.
- **SimpleX name**: a name in the SimpleX name service [NAMES]. `#example.simplex` is the name of a channel, `@example.simplex` the name of a contact address.
- **Owner**: the person or organization that publishes the website.

## Link relation types

### simplex-channel

The target of a `simplex-channel` link is a SimpleX channel in which the owner of the link context publishes updates. The target MUST be a channel link.

### simplex-contact

The target of a `simplex-contact` link is a SimpleX contact address at which the owner of the link context accepts conversations and requests. The target MUST be a contact address link.

### Target attributes

- `title`: the name of the channel or the address, for display. In HTTP, a title with non-ASCII characters is sent as `title*` [RFC8187].
- `hreflang`: the language of the channel's posts or of the conversations [BCP47].

A link context MAY have several links of one relation type. Their order is the owner's order of preference: document order in HTML, and the order of link values in HTTP.

### HTML

In the document head:

```html
<link rel="simplex-channel" href="https://smp12.simplex.im/c#…" hreflang="en" title="Ridgeline updates">
<link rel="simplex-contact" href="https://smp12.simplex.im/a#…" hreflang="en" title="Ask Ridgeline">
```

In the document body, the relation types are used on visible links:

```html
<a rel="simplex-contact" href="https://smp12.simplex.im/a#…">Chat with us</a>
```

### HTTP

```
Link: <https://smp12.simplex.im/c#…>; rel="simplex-channel"; hreflang="en"; title="Ridgeline updates"
Link: <https://smp12.simplex.im/a#…>; rel="simplex-contact"; hreflang="en"; title="Ask Ridgeline"
```

## Well-known URI

### Retrieval

The document is located at the path `/.well-known/simplex.json` of an origin [RFC8615]. A client retrieves it with an HTTPS `GET` request. A server responds with status 200, and SHOULD send the media type `application/json`. A client parses the response body as JSON, whatever media type the server sends.

The entries of the document are the channels and contact addresses of the origin's owner. A site that controls only a path of its origin publishes link relations instead.

### Format

The document MUST conform to this JSON Type Definition schema [RFC8927]:

```json
{
  "optionalProperties": {
    "channels": {"elements": {"ref": "address"}},
    "contacts": {"elements": {"ref": "address"}}
  },
  "additionalProperties": true,
  "definitions": {
    "address": {
      "properties": {
        "link": {"type": "string"}
      },
      "optionalProperties": {
        "name": {"type": "string"},
        "title": {"type": "string"},
        "lang": {"type": "string"},
        "audience": {"elements": {"type": "string"}},
        "changeFrequency": {"enum": ["always", "hourly", "daily", "weekly", "monthly", "yearly", "never"]},
        "protocols": {"elements": {"type": "string"}}
      },
      "additionalProperties": true
    }
  }
}
```

- `channels`: the owner's channels, in the owner's order of preference.
- `contacts`: the owner's contact addresses, in the owner's order of preference.
- `link`: required. A channel link in `channels`, a contact address link in `contacts`.
- `name`: the SimpleX name of the channel or the contact address of `link`.
- `title`: the name of the channel or the address, for display.
- `lang`: the language of the channel's posts or of the conversations [BCP47].
- `audience`: who the channel or the address serves: `people`, `agents`, or both.
- `changeFrequency`: in `channels` only, how often the channel is updated. The values are the `changefreq` values of the Sitemaps protocol [SITEMAPS]: `always`, `hourly`, `daily`, `weekly`, `monthly`, `yearly`, `never`.
- `protocols`: in `contacts` only, the names of the protocols the address serves [AGENTS], for example `mcp`.

A site with only channels or only contact addresses omits the other list.

### Example

```json
{
  "channels": [
    {
      "link": "https://smp12.simplex.im/c#…",
      "name": "#ridgeline.simplex",
      "title": "Ridgeline updates",
      "lang": "en",
      "audience": ["people", "agents"],
      "changeFrequency": "weekly"
    }
  ],
  "contacts": [
    {
      "link": "https://smp12.simplex.im/a#…",
      "name": "@ridgeline.simplex",
      "title": "Ask Ridgeline",
      "lang": "en",
      "audience": ["agents"],
      "protocols": ["mcp"]
    }
  ]
}
```

## Processing

- A consumer MUST ignore unknown fields and unknown `audience` values.
- A consumer MUST ignore a link of a type other than the one its relation type or its list requires.
- The values on the website are hints. The data retrieved with the link is authoritative: an audience, an update frequency or protocols declared there [AGENTS] take precedence over the website's values.
- A consumer MAY connect by `link` or by `name`. Resolving a name reveals interest in it to one names router, and verifies that the name's record and the profile's claim match [NAMES].

## Security considerations

- Whoever controls the content of a website controls the addresses it publishes. Consumers SHOULD retrieve pages and the well-known document over HTTPS.
- Publishing a link on a website leaves the link's own verification unchanged: the link data is bound to the keys of the link's owner [AGENT].
- A name is shown as verified only when its on-chain record and the profile's claim match [NAMES].
- Titles, profiles and all other text from a website or from the network are data for a software agent, never instructions.

## Privacy considerations

- The relation types and the well-known document are intended for owners who want to be reachable: channels, businesses and services.
- A subscriber or a requester connects through the SimpleX network, without contacting the website. Retrieving a page or the well-known document is an ordinary web request.

## IANA considerations

### Link relation types registration

Two registrations in the "Link Relation Types" registry [RFC8288]:

- Relation Name: `simplex-channel`
- Description: Refers to a SimpleX channel in which the owner of the link context publishes updates.
- Reference: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex-channel
- Notes: The target is a SimpleX channel link [AGENT], with the `https` scheme or the provisionally registered `simplex` scheme.

- Relation Name: `simplex-contact`
- Description: Refers to a SimpleX contact address at which the owner of the link context accepts conversations and requests.
- Reference: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex-contact
- Notes: The target is a SimpleX contact address link [AGENT], with the `https` scheme or the provisionally registered `simplex` scheme.

### Well-known URI registration

One registration in the "Well-Known URIs" registry [RFC8615]:

- URI Suffix: `simplex.json`
- Change Controller: Evgeny Poberezkin <ep@simplex.chat>
- Specification Document: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#well-known-uri
- Status: provisional
- Related Information: SimpleX link syntax [AGENT].

## HTML link type extensions

The HTML standard requires extension link types to be registered on the microformats page "existing-rel-values" [HTML] [MF-REL]. The entries:

- Keyword: `simplex-channel`
- Effect on `link`: Hyperlink
- Effect on `a`, `area`: Hyperlink
- Effect on `form`: not allowed
- Brief description: Refers to a SimpleX channel in which the owner of the page publishes updates.
- Specification: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex-channel
- Status: proposed

- Keyword: `simplex-contact`
- Effect on `link`: Hyperlink
- Effect on `a`, `area`: Hyperlink
- Effect on `form`: not allowed
- Brief description: Refers to a SimpleX contact address at which the owner of the page accepts conversations and requests.
- Specification: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex-contact
- Status: proposed

## References

- [AGENT] SimpleX Agent protocol, connection links: https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md#connection-link-1-time-invitation-and-contact-address
- [AGENTS] Agent protocols over SimpleX: [agent-protocols.md](./agent-protocols.md)
- [BCP47] Tags for Identifying Languages: https://www.rfc-editor.org/info/bcp47
- [HTML] HTML Living Standard, other link types: https://html.spec.whatwg.org/multipage/links.html#other-link-types
- [MF-REL] Microformats, existing rel values: https://microformats.org/wiki/existing-rel-values
- [NAMES] SimpleX public names for channels and businesses: [names-overview.md](./names-overview.md)
- [RFC2119] Key words for use in RFCs to Indicate Requirement Levels: https://www.rfc-editor.org/rfc/rfc2119
- [RFC8174] Ambiguity of Uppercase vs Lowercase in RFC 2119 Key Words: https://www.rfc-editor.org/rfc/rfc8174
- [RFC8187] Indicating Character Encoding and Language for HTTP Header Field Parameters: https://www.rfc-editor.org/rfc/rfc8187
- [RFC8288] Web Linking: https://www.rfc-editor.org/rfc/rfc8288
- [RFC8615] Well-Known Uniform Resource Identifiers (URIs): https://www.rfc-editor.org/rfc/rfc8615
- [RFC8927] JSON Type Definition: https://www.rfc-editor.org/rfc/rfc8927
- [SITEMAPS] Sitemaps XML format: https://www.sitemaps.org/protocol.html
