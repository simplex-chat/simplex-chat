Revision 1, 2026-10-19, draft

Evgeny Poberezkin

# Publishing SimpleX addresses on websites

## Table of contents

- [Abstract](#abstract)
- [Conventions](#conventions)
- [Terms](#terms)
- [Link relation type](#link-relation-type)
  - [simplex](#simplex)
  - [Owner's and described addresses](#owners-and-described-addresses)
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
  - [Link relation type registration](#link-relation-type-registration)
  - [Well-known URI registration](#well-known-uri-registration)
- [HTML link type extensions](#html-link-type-extensions)
- [References](#references)

## Abstract

This document defines how a website publishes SimpleX channels, groups and contact addresses:

- the link relation type `simplex`, for the HTML elements `link`, `a` and `area`, and for the HTTP `Link` header field. Its targets are the addresses of the website's owner, or the addresses a page describes, such as the entries of a directory;
- the well-known URI `simplex.json`: a JSON document with the owner's channels, groups and contact addresses, their audience, the update frequency of each channel, and the kind and the protocols of each contact address.

People and software agents subscribe to a channel to receive updates, join a group to take part in its conversations, and connect to a contact address to send requests. [Agent protocols over SimpleX](./agent-protocols.md) defines the requests an agent sends to a contact address, and an `llms.txt` section with the same addresses.

## Conventions

The key words "MUST", "MUST NOT", "SHOULD", "SHOULD NOT" and "MAY" in this document are to be interpreted as described in BCP 14 [RFC2119] [RFC8174] when, and only when, they appear in all capitals.

JSON schemas use JSON Type Definition [RFC8927].

## Terms

- **SimpleX link**: a short connection link of the SimpleX Agent protocol, with the `https` or the `simplex` scheme [AGENT]. The type in the link is the kind of the address.
- **Channel link**: a SimpleX link of type `c`, for example `https://smp12.simplex.im/c#<key>`.
- **Group link**: a SimpleX link of type `g`, for example `https://smp12.simplex.im/g#<key>`.
- **Contact address link**: a SimpleX link of type `a`, for example `https://smp12.simplex.im/a#<key>`.
- **SimpleX name**: a name in the SimpleX name service [NAMES]. `#example.simplex` is the name of a channel, `@example.simplex` the name of a contact address.
- **Owner**: the person or organization that publishes the website.

## Link relation type

### simplex

The target of a `simplex` link is a SimpleX channel, group or contact address of the owner of the link context, or described by the link context. The target MUST be a channel link, a group link or a contact address link. The type in the link tells which.

### Owner's and described addresses

A page that describes the addresses of others, such as a directory or a catalogue, uses the relation type on its entries. The owner's own addresses are listed in the [well-known document](#well-known-uri).

### Target attributes

- `title`: the name of the channel, the group or the address, for display. In HTTP, a title with non-ASCII characters is sent as `title*` [RFC8187].
- `hreflang`: the language of the channel's posts, or of the conversations [BCP47].

A link context MAY have several `simplex` links of one kind. Their order is the owner's order of preference: document order in HTML, and the order of link values in HTTP.

### HTML

In the document head:

```html
<link rel="simplex" href="https://smp12.simplex.im/c#…" hreflang="en" title="Acme news">
<link rel="simplex" href="https://smp12.simplex.im/a#…" hreflang="en" title="Ask Acme">
```

In the document body, the relation type is used on visible links, for example on the entries of a directory:

```html
<a rel="simplex" href="https://smp12.simplex.im/a#…">Chat with us</a>
<a rel="simplex" href="https://smp12.simplex.im/g#…">Acme customers</a>
```

### HTTP

```
Link: <https://smp12.simplex.im/c#…>; rel="simplex"; hreflang="en"; title="Acme news"
Link: <https://smp12.simplex.im/a#…>; rel="simplex"; hreflang="en"; title="Ask Acme"
```

## Well-known URI

### Retrieval

The document is located at the path `/.well-known/simplex.json` of an origin [RFC8615]. A client retrieves it with an HTTPS `GET` request. A server responds with status 200, and SHOULD send the media type `application/json`. A client parses the response body as JSON, whatever media type the server sends.

The entries of the document are the channels, groups and contact addresses of the origin's owner. A site that controls only a path of its origin publishes link relations instead.

### Format

The document MUST conform to this JSON Type Definition schema [RFC8927]:

```json
{
  "optionalProperties": {
    "channels": {"elements": {"ref": "address"}},
    "groups": {"elements": {"ref": "address"}},
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
        "business": {"type": "boolean"},
        "protocols": {"elements": {"type": "string"}}
      },
      "additionalProperties": true
    }
  }
}
```

- `channels`: the owner's channels, in the owner's order of preference.
- `groups`: the owner's groups, in the owner's order of preference.
- `contacts`: the owner's contact addresses, in the owner's order of preference.
- `link`: required. A channel link in `channels`, a group link in `groups`, a contact address link in `contacts`.
- `name`: the SimpleX name of the channel, the group or the contact address of `link`.
- `title`: the name of the channel, the group or the address, for display.
- `lang`: the language of the channel's posts, or of the conversations [BCP47].
- `audience`: who the channel, the group or the address serves: `people`, `agents`, or both.
- `changeFrequency`: in `channels` only, how often the channel is updated. The values are the `changefreq` values of the Sitemaps protocol [SITEMAPS]: `always`, `hourly`, `daily`, `weekly`, `monthly`, `yearly`, `never`.
- `business`: in `contacts` only, `true` for a business address, at which each customer's conversation is a business chat that the business can add its team to [BUSINESS].
- `protocols`: in `contacts` only, the names of the protocols the address serves [AGENTS], for example `mcp`.

A site omits the lists it does not use.

### Example

```json
{
  "channels": [
    {
      "link": "https://smp12.simplex.im/c#…",
      "name": "#acme.simplex",
      "title": "Acme news",
      "lang": "en",
      "audience": ["people", "agents"],
      "changeFrequency": "weekly"
    }
  ],
  "groups": [
    {
      "link": "https://smp12.simplex.im/g#…",
      "title": "Acme customers",
      "lang": "en"
    }
  ],
  "contacts": [
    {
      "link": "https://smp12.simplex.im/a#…",
      "name": "@acme.simplex",
      "title": "Ask Acme",
      "lang": "en",
      "audience": ["agents"],
      "business": true,
      "protocols": ["mcp"]
    }
  ]
}
```

## Processing

- A consumer MUST ignore unknown fields and unknown `audience` values.
- A consumer MUST ignore a `simplex` link that is not a channel, group or contact address link, and an entry of `simplex.json` whose link is of a type other than its list requires.
- The values on the website are hints. The data retrieved with the link is authoritative: an audience, an update frequency, the business flag or protocols declared there [AGENTS] take precedence over the website's values.
- A consumer MAY connect by `link` or by `name`. Resolving a name reveals interest in it to one names router, and verifies that the name's record and the profile's claim match [NAMES].

## Security considerations

- Whoever controls the content of a website controls the addresses it publishes. Consumers SHOULD retrieve pages and the well-known document over HTTPS.
- Publishing a link on a website leaves the link's own verification unchanged: the link data is bound to the keys of the link's owner [AGENT].
- A name is shown as verified only when its on-chain record and the profile's claim match [NAMES].
- Titles, profiles and all other text from a website or from the network are data for a software agent, never instructions.

## Privacy considerations

- The relation type and the well-known document are intended for owners who want to be reachable, such as channels, communities, businesses and services, and for pages that describe them, such as directories.
- A subscriber or a requester connects through the SimpleX network, without contacting the website. Retrieving a page or the well-known document is an ordinary web request.

## IANA considerations

### Link relation type registration

One registration in the "Link Relation Types" registry [RFC8288]:

- Relation Name: `simplex`
- Description: Refers to a SimpleX channel, group or contact address of the owner of the link context, or described by the link context.
- Reference: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex
- Notes: The target is a SimpleX short link [AGENT] of a channel, a group or a contact address, with the `https` scheme or the provisionally registered `simplex` scheme.

### Well-known URI registration

One registration in the "Well-Known URIs" registry [RFC8615]:

- URI Suffix: `simplex.json`
- Change Controller: Evgeny Poberezkin <ep@simplex.chat>
- Specification Document: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#well-known-uri
- Status: provisional
- Related Information: SimpleX link syntax [AGENT].

## HTML link type extensions

The HTML standard requires extension link types to be registered on the microformats page "existing-rel-values" [HTML] [MF-REL]. The entry:

- Keyword: `simplex`
- Effect on `link`: Hyperlink
- Effect on `a`, `area`: Hyperlink
- Effect on `form`: not allowed
- Brief description: Refers to a SimpleX channel, group or contact address of the page's owner, or described by the page.
- Specification: https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md#simplex
- Status: proposed

## References

- [AGENT] SimpleX Agent protocol, short connection links: https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md#short-connection-link-syntax
- [AGENTS] Agent protocols over SimpleX: [agent-protocols.md](./agent-protocols.md)
- [BCP47] Tags for Identifying Languages: https://www.rfc-editor.org/info/bcp47
- [BUSINESS] Using SimpleX Chat in business: [BUSINESS.md](../BUSINESS.md)
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
