---
title: Registering a SimpleX name
---

# Registering a SimpleX name

A SimpleX name lets people reach you by typing a short `@name` instead of pasting
a long link. You register the name on a website and add your SimpleX link to it,
then claim the name in the app, to connect it with your SimpleX address or
channel.

> Names currently run on a testing network, and test names end in `.testing`.
> The registration page is
> [testing-names.simplex.chat](https://testing-names.simplex.chat). In the app,
> **How to register a test name** on the SimpleX name screen of [Step
> 4](#step-4-claim-the-name-in-the-app) opens
> [simplex.domains/#testing](https://simplex.domains/#testing), with links to
> the registration page and to this guide.
>
> Test names can be used in the app until the SimpleX name sale starts, on 12
> December 2026 at 18:00 UTC. The app counts down to the sale under **SimpleX
> name sale starts in**.

## What you need

- A crypto wallet such as MetaMask.
- The SimpleX app.
- A link for the name to open: your **contact address** (for one-to-one chats),
  your **channel link** (to join a channel), or both.

## How it works

Setting up a name takes two parts. On the website you register the name and add
your link, so people can find your link by name. In the app you set the same name
on your profile - to prevent any other names from opening your profile or channel.

![Steps to register a SimpleX name](./diagrams/simplex-name-steps.svg)

## Step 1. Create a wallet

Install MetaMask (browser extension or mobile app) and create a wallet. Write
down your recovery phrase somewhere safe and offline, as anyone with that phrase
controls your names.

On the testing network you need a small amount of test ETH to pay network fees.

## Step 2. Register the name and add your link

Open the registration page and tap **Connect** to link your wallet. Type the
name you want in **Search for a name** and open it. Choose how many years to
register for, then tap **Next**. Names are currently 6 characters or more, and
some names are reserved.

![Register your name and choose the registration period](./images/register-name.png)

On **Create your profile**, paste your SimpleX link into the matching field:

![Create your profile](./images/create-your-profile.png)

- **SimpleX contact** for one-to-one chats. Copy your address from the app
  (**Create SimpleX address**) and paste it here. People reach it with `@name`.
- **SimpleX channel** for a channel. Copy your **Channel link** and paste it here.
  People join it with `#name`.
- **Advanced usage:** fill both to use one name for your chat (`@name`) and your
  channel (`#name`).

Then tap **Next**.

Finally, tap **Begin** and complete the three steps the site shows:

> Before we start. Registering your name takes three steps:
> 1. Complete a transaction to begin the timer.
> 2. Wait 60 seconds for the timer to complete.
> 3. Complete a second transaction to secure your name.

These two transactions protect the name you are registering. The first records
only a secret code for the name, not the name itself, so nobody watching the
blockchain can see which name you want. The wait lets that first transaction
settle, and the second transaction then reveals and claims the name. Without this
two-step process, someone could see your registration in progress and grab the
name before you.

![The three steps to secure your name](./images/before-we-start.png)

## Step 3. Check what the app sees

Copy your `#name.testing` (or `@name.testing`). Paste the name into the search
field of the chat list, or type it, and tap **Connect to …**. When you type a
name of 5 or more characters without a dot, the app adds `.testing` to it.

The app reports that the name is registered but not yet added to your profile:

> Unconfirmed name. The SimpleX name is registered, but not added to profile.
> Please add it to your address or channel profile, if you are the owner.

This is expected, and confirms the name points at your link. The next step
finishes the setup.

## Step 4. Claim the name in the app

Set the name on the same profile the record points at.

- For your **contact address**, open [your SimpleX
  address](./making-connections.md#your-simplex-contact-address), tap **Get
  SimpleX name (BETA)**, enter the full name, for example `@name.testing`, and
  tap **Save**. The app sends the profile update to your contacts.
- For your **channel**, open the channel's chat info, tap **Get SimpleX name
  (BETA)** below the channel link, enter the full name, for example
  `#name.testing`, and tap **Save**. This button is shown to the channel owner.

Enter the name with `.testing`: a name entered without a dot is saved with
`.simplex`. The saved name is then shown under **Your SimpleX name** in your
address, or under **Channel SimpleX name** in the channel's chat info.

This proves to people who connect to you that `#name.testing` (or `@name.testing`) is your name.

## Step 5. Connect by name

On another device, copy `#name.testing` (or `@name.testing`). Paste the name
into the search field, or type it, and tap **Connect to …**. This time the app
shows the channel or the contact: tap **Open channel** or **Open new chat**. A
check mark is shown next to the name.

You can paste the name in your own device too - the app will warn you that it is your own name.

Anyone can now reach you by name, and their app confirms it is really you.

A contact's or channel's SimpleX name can be shown in its chat info. A check
mark next to the name means that the app verified that the name is registered
with this contact's address or this channel's link. Next to a name that is not
verified yet, tap **Verify name** to check it; a red cross is shown when the
name is not registered with this address or link. To verify names whenever you
open the chat info, turn on **Verify SimpleX names** in **Settings** > **Your
privacy** > **More privacy**; it is off by default.

## If something doesn't work

| Message | Meaning | Fix |
|---|---|---|
| Unconfirmed name | The name points at a link, but the link's profile does not claim the name yet. | Do step 4: set the name on the profile it points at. |
| Name not found | No name is registered. | Check the spelling, or register it (step 2). |
| No valid link | The name has no contact or channel link. | Add a link to the name on the website, on its **Records** tab. |
| Error saving name | You tried to claim a name that has no matching link on the website. | Add your contact address or channel link to the name, then set the name again. |
| None of your servers are set to resolve SimpleX names | No server in the app is set to resolve names. | In **Network & servers**, open a server operator and turn on **To resolve names** under **Use for messages**, or set **To resolve names** for one of the servers in **Your servers**. |

## See also

- [Making connections](./making-connections.md)
- [Chat profiles](./chat-profiles.md)
- [Channels](./channels.md)
- [Channel webpage](./channel-webpage.md)
