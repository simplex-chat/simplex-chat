---
title: Making connections
---
# Connect to somebody in the chat

Because you have no identifier on the SimpleX network, nobody can contact you
unless you share a 1-time link or your SimpleX address, as a QR code or a
link, or a SimpleX name registered for your address. Members of groups you join
can also send you contact requests, when direct messages are allowed in the
group.

Your SimpleX address is optional. You can change or delete it without losing any
of your connections, for example when you receive spam contact requests.

Private Connection — connect using a 1-time link, or its QR code shown in person
or in a video call.

Group Chat — join a group via its link or an invitation from a contact, or
create your own group, see [Groups](./secret-groups.md).

Channel — join a channel via its link or its SimpleX name, see
[Channels](./channels.md).

To find public groups and channels, see [Find groups and
channels](#find-groups-and-channels).

## Your SimpleX contact address

You can [create an optional long term
address](./app-settings.md#your-simplex-contact-address) for other people to
connect with you. Unlike 1-time invitation links, these addresses can be used
many times, that makes them good to share online, e.g. on social media
platforms, or in email signatures.

When people connect to you via this address, you will receive a connection
request that you can accept or reject. You can configure an automatic acceptance
of connection request and an automatic welcome message that will be sent to the
new contacts. You can also share this address as part of your SimpleX profile,
so group members can connect to you (in the groups that allow direct messages
and SimpleX links), and your contacts can share it with others - if this is
something that you want.

If you start receiving too many requests via this address it is always safe to remove it – all the connections you created via this address will remain active, as this address is not used to deliver the messages.

### Comparison of 1-time invitation links and SimpleX Contact addresses

<table>
  <tr>
    <th></th>
    <th>1-time invitation link</th>
    <th>SimpleX contact address</th>
  </tr>
  <tr>
    <td>Can be used many times?</td>
    <td>No</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td>Can be included in user profile?</td>
    <td>No, as it can only be used once.</td>
    <td>Yes, to allow group members to connect directly, and your contacts to pass it on to their contacts.</td>
  </tr>
  <tr>
    <td>When to use it?</td>
    <td>With somebody you know, via another communication channel or QR code (in person or during a video call)</td>
    <td>Where many people can see and connect via it, e.g. in email signature, website, social media or group chat.</td>
  </tr>
  <tr>
    <td>Security</td>
    <td>More secure, as can only be used once, and the initial connection request (including profile) is encrypted with double ratchet.</td>
    <td>Initial connection request is also e2e encrypted, but without double ratchet (it is initialized when request is accepted).</td>
  </tr>
  <tr>
    <td>Identification</td>
    <td>Both sides know who they connect to, as they know with whom and by who the link was shared. You can set a contact name for this invitation as soon as you share it or use it, to identify the other person when connection is established.</td>
    <td>Only the person using the address knows who they connect to, via the channel where they found the address (email, social media, etc.). The address owner can only see the user profile of the request, and has no proof of identity from the person sending the request.</td>
  </tr>
  <tr>
    <td>Advantages over other platforms</td>
    <td>There is no direct analogy, other platforms don’t offer one-time invitations without any fixed part identifying the user.</td>
    <td>Unlike addresses in other platforms, SimpleX addresses are not used to deliver the messages &mdash; only the initial connection requests.<br>It means that removing this address will not break the contacts made via it (like changing an email address would), it would only prevent new connections, which makes it a good solution against spam and abuse.</td>
  </tr>
  <tr>
    <td>Vulnerability to attacks</td>
    <td>Until the connection is established, anybody who intercepts this link can connect to it, so it has to be verified with the original contact that the connection succeeded.</td>
    <td>These addresses are vulnerable to connection request spam. Unlike other platforms, you can delete or change the address, without losing any contacts (see above).</td>
  </tr>
  <tr>
    <td>Passive attacks on connection links</td>
    <td colspan="2">Both types of links are not vulnerable if simply observed &mdash; the private keys are kept on the devices and are not included in the links. So they can be safely shared via insecure or public channels, as long as you can confirm that you connected to the intended person.</td>
  </tr>
  <tr>
    <td>Active attacks on connection links</td>
    <td colspan="2">If the link is substituted via the attack on the channel used to share it, the connection security can be compromised, and the original messages monitored (man-in-the-middle attack). If it is a real risk then security code should be verified to mitigate it - doing so proves<sup>**</sup> that the link and keys were not substituted, and that the end-to-end encryption is secure.</td>
  </tr>
</table>

<sup>**</sup> Connection security code is the cryptographic hash (SHA256) of combined public keys of both sides &mdash; there are 2<sup>256</sup> possible security codes (1 with 77 zeros – about [1000 times smaller](https://www.wolframalpha.com/input?i=2%5E256) than the estimated number of atoms in the visible universe).

## Connect via a link or a name

To connect via a 1-time link or a SimpleX address that you received:

- Tap the pencil button, then "Scan / Paste link" (iOS), "Paste link / Scan"
  (Android) or "Paste link" (desktop), and paste the link, or scan its QR code
  on iOS and Android. You can also paste the link into the search field of the
  chat list, or tap the link in a message.
- The app shows the profile included in the link. Tap "Open new chat".
- In the new chat, "Your profile" shows the profile you will connect with. Tap
  it to choose another chat profile or "Incognito", see [Incognito
  mode](./chat-profiles.md#incognito-mode).
- Tap "Connect". For an address, you can also send a message with your contact
  request. More messages can be sent after the request is accepted.

For some links, the app shows no profile and asks you to choose "Use current
profile" or "Use new incognito profile".

To connect via a SimpleX name, type it into the search field of the chat list,
for example `@name.testing`, and tap the "Connect to …" row, see [Registering a
SimpleX name](./register-simplex-name.md).

## Find groups and channels

To find public groups and channels in SimpleX Directory:

- Type what you are looking for into the search field of the chat list, or of
  New chat (tap the pencil button).
- Tap "Search in Directory", or use the Search key of the keyboard (Enter on
  desktop).
- While "Directory search" is off, the app asks "Enable Directory search?". The
  search text is sent to SimpleX Directory, each search separately and without
  your profile. Tap "Enable" to search, or "Cancel" to send nothing. You can
  change it later, see [Directory
  search](./app-settings.md#directory-search).

The results show after your matching chats. Tap a result or its "Open" to open
the group or channel, then tap "Join group" or "Join channel". Tap "Load more"
for the next results. A group or channel you joined by the link SimpleX
Directory lists shows as your chat. A group you were invited to, or joined by
another link, can also show as a result, and opening that result can add a
second copy of the group to your chats.

Results are not saved: they go away when you change the search text or switch
chat profile.

## Conversation preferences

Open one of your conversations, tap the contact's name in the chat toolbar and
tap "Contact preferences". Here you can choose whether you wish to allow
disappearing messages, deleting messages for everyone, message reactions, voice
messages and audio/video calls.
