---
title: Channel webpage
---
# Channel webpage

A channel webpage shows a preview of your channel on the web: its name, description, recent messages and subscriber count. Visitors can see what the channel is about before they subscribe, and the page gives them a "Join" button along with links to download the app.

You don't have to build the preview yourself. The chat relays that host your channel publish its content as a small file, and a ready-made script renders it on your page. All it takes is to copy the code the app generates and paste it into a web page you host.

## What you need

A channel webpage can be set up by the **owner** of the channel, as long as the channel is hosted on chat relays that support webpages. If they don't, the app shows "Used chat relays do not support webpages." and no code is generated.

You'll also need somewhere to publish an HTML page. Your own site works, but any static hosting will do.

## Step 1. Open the channel webpage settings

Open the channel and tap its name at the top to open the channel information. Scroll down to **Advanced options** and tap **Channel webpage**. This button is only shown to channel owners.

## Step 2. Allow embedding while you build the page

Turn on **Allow anyone to embed** and tap **Save**.

With this on, the relay serves your channel preview to any page, so you can build and test from wherever the page lives without it being tied to one domain yet. Leave the webpage URL empty for now; nothing is shown to your subscribers until you set it.

## Step 3. Copy the code

Under **Webpage code** you'll see a snippet like the one below. Tap **Copy code**.

```html
<div data-simplex-channel-preview
  data-channel-link="https://smp15.simplex.im/c#Q-uRqD9tDixMf4Cd6UByeNN5nyIKkrgs4WaLlTnbjXs"
  data-channel-id="kFXapqMZpdFGZFKXDibQPpDHV4aeA4x9dUk-PNKl1LA="
  data-relay-domains="relay1.example.com"
  data-app-download-buttons="on"
  data-color-scheme="light"
></div>
<script src="https://simplex.chat/js/channel-preview.js"></script>
```

Everything specific to your channel is already in the code: its link, its ID, and the relay domains that serve the preview. There's no need to edit those values.

## Step 4. Add the code to your page and test it

Paste the snippet into the page you're going to publish. Here's a complete minimal page:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>My Channel</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    html, body { height: 100%; }
    [data-simplex-channel-preview] { height: 100%; }
  </style>
</head>
<body>
  <div data-simplex-channel-preview
    data-channel-link="https://smp15.simplex.im/c#Q-uRqD9tDixMf4Cd6UByeNN5nyIKkrgs4WaLlTnbjXs"
    data-channel-id="kFXapqMZpdFGZFKXDibQPpDHV4aeA4x9dUk-PNKl1LA="
    data-relay-domains="relay1.example.com"
    data-app-download-buttons="on"
  ></div>
  <script src="https://simplex.chat/js/channel-preview.js"></script>
</body>
</html>
```

Publish the page and open it in a browser. The channel preview shows up in place of the `<div>`. Because embedding is still open, it loads no matter which address you test from, so you can adjust the page until it looks right.

## Step 5. Set the webpage URL and lock it down

Once the page works, go back to **Channel webpage** in the app:

- Under **Enter webpage URL**, type the address where the page is published, for example `https://example.com/my-channel`.
- Turn **Allow anyone to embed** off if you don't want other sites to be able to show your channel preview. Leave it on if you're happy for anyone to embed it.
- Tap **Save**.

The URL now appears as a link in your channel info that every subscriber can see. If you turned embedding off, the relay also restricts the preview to your own domain.

## Customizing the preview

You can add optional `data-*` attributes to the `<div>` to change how it looks. Only `data-channel-id` and `data-relay-domains` are required, and both are already in the generated code.

| Attribute | Values | Default | What it does |
| --- | --- | --- | --- |
| `data-channel-id` | channel ID (from the app) | none | Required. Identifies the channel to load. |
| `data-relay-domains` | comma-separated domains | none | Required. Relay domains that serve the preview, tried in order. |
| `data-channel-link` | channel link | none | Enables the "Join" button and QR code. Recommended. |
| `data-app-download-buttons` | `on`, `off` | `on` | Shows or hides the app download buttons. |
| `data-color-scheme` | `light`, `dark`, `site` | `light` | Color theme. `site` follows your page's theme (it uses dark styling when a parent element has the `dark` CSS class). |
| `data-light-background` | CSS color | `#ffffff` | Background color in light mode. |
| `data-dark-background` | CSS color | `#000832` | Background color in dark mode. |
| `data-relay-scheme` | `https`, `http` | `https` | Protocol used to load the preview from the relays. Leave it as `https`. |

For example, here's a dark theme with a custom background and the download buttons hidden:

```html
<div data-simplex-channel-preview
  data-channel-link="https://smp15.simplex.im/c#Q-uRqD9tDixMf4Cd6UByeNN5nyIKkrgs4WaLlTnbjXs"
  data-channel-id="kFXapqMZpdFGZFKXDibQPpDHV4aeA4x9dUk-PNKl1LA="
  data-relay-domains="relay1.example.com,relay2.example.com"
  data-app-download-buttons="off"
  data-color-scheme="dark"
  data-dark-background="#001a4d"
></div>
<script src="https://simplex.chat/js/channel-preview.js"></script>
```

## Good to know

The preview updates on its own. The relays republish channel content periodically, so the page picks up new messages without any change on your side. It's a read-only snapshot, so visitors can't post to it.

Only what the channel already shows publicly is included: recent messages, member display names and avatars, reactions, and the subscriber count. Deleted and disappearing messages are never published.

If your channel is served by more than one relay, all of them are listed in `data-relay-domains`. The script tries them in order, so the preview still loads when one relay is unavailable.

## If something doesn't work

If the preview area stays empty, check that the page is hosted on the same domain as the URL you set in Step 5, or turn **Allow anyone to embed** back on while you sort it out. The relay only lets that domain load the preview.

If the app says "Used chat relays do not support webpages.", the relays hosting your channel don't support this feature yet, so no code can be generated.

If there's no **Channel webpage** button, remember that it only appears for channel owners on channels hosted on relays.
