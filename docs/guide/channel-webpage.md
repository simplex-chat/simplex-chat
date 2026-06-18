---
title: Channel webpage
---
# Channel webpage

A channel webpage shows a preview of your channel on the web: its name, description, recent messages and subscriber count. Visitors can see what the channel is about before they subscribe, and the page gives them a "Join" button along with links to download the app.

You don't have to build the preview yourself. The chat relays that host your channel publish its content as a small file, and a ready-made script renders it on your page. All it takes is to enter the address of your page in the app, copy the code it generates, and paste that code into a web page you host.

## What you need

A channel webpage can be set up by the **owner** of the channel, as long as the channel is hosted on chat relays that support webpages. If they don't, the app shows "Used chat relays do not support webpages." and no code is generated.

You'll also need somewhere to publish an HTML page. Your own site works, but any static hosting will do.

## Step 1. Open the channel webpage settings

Open the channel and tap its name at the top to open the channel information. Scroll down to **Advanced options** and tap **Channel webpage**. This button is only shown to channel owners.

## Step 2. Enter the webpage URL

Under **Enter webpage URL**, type the address where you're going to publish the page, for example `https://example.com/my-channel`.

This address matters. It's shown to your subscribers, and the relay uses its domain to allow your page to load the preview. If you publish the page on a different domain, the preview won't load there (unless you allow embedding everywhere, which is the next step).

## Step 3. Choose who can embed the preview

The **Allow anyone to embed** toggle controls which sites are allowed to show your channel preview.

When it's off (the default), only the page at the URL above can show the preview. When it's on, any webpage can show it. Turn it on only if you actually want other sites to embed your channel.

## Step 4. Copy the code and save

Under **Webpage code** you'll see a snippet like the one below. Tap **Copy code**, then tap **Save**. The app asks you to **Save and notify channel subscribers**; confirm to apply the settings.

The generated code looks like this:

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

## Step 5. Add the code to your webpage

Paste the snippet into the page you host at the URL from Step 2. Here's a complete minimal page:

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

Publish the page and open it in a browser. The channel preview shows up in place of the `<div>`.

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

If the preview area stays empty, check that the page is hosted on the same domain as the URL you entered in Step 2, or turn on **Allow anyone to embed**. The relay only lets that domain load the preview.

If the app says "Used chat relays do not support webpages.", the relays hosting your channel don't support this feature yet, so no code can be generated.

If there's no **Channel webpage** button, remember that it only appears for channel owners on channels hosted on relays.
