---
title: Making connections
---
# Connect to somebody in the chat

_Work in progress_

Because you have no identifier on the SimpleX platform, nobody can contact you unless you share a one-time or temporary user address, such as a QR code or a link.

Even with the optional user address, while it can be used to send spam contact requests, you can change or completely delete it without losing any of your connections.

Private Connection — connect using an invitation link or QR code via video or in-person.

Group Chat — Users have the option to create a secret group, share their contact link [which can be deleted later on], or generate a one-time invitation link.

## Your SimpleX contact address

You can [create an optional long term address](./app-settings.md#your-simplex-contact-address) for other people to connect with you. Unlike 1-time invitation links, these addresses can be used many times, that makes them good to share online, e.g. on social media platforms, or in email signatures. That helps more people discover SimpleX Chat, so please do it!

When people connect to you via this address, you will receive a connection request that you can accept or reject. You can configure an automatic acceptance of connection request and an automatic welcome message that will be sent to the new contacts. You can also share this address as part of your SimpleX profile, so group members can connect to you, and your contacts can share it with others - if this is something that you want.

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
    <td>Both sides know who they connect to, as they know with whom and by who the link was shared. You can attach alias to this invitation as soon as you share it or use it, to identify the other person when connection is established.</td>
    <td>Only the person using the address knows who they connect to, via the channel where they found the address (email, social media, etc.). The address owner can only see the user profile of the request, and has no proof of identity from the person sending the request<sup>*</sup>.</td>
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
    <td colspan="2">Both types of links are not vulnerable if simply observed &mdash; they only contain public keys. So they can be safely shared via insecure or public channels, as long as you can confirm that you connected to the intended person.</td>
  </tr>
  <tr>
    <td>Active attacks on connection links</td>
    <td colspan="2">If the link is substituted via the attack on the channel used to share it, the connection security can be compromised, and the original messages monitored (man-in-the-middle attack). If it is a real risk then security code should be verified to mitigate it - doing so proves<sup>**</sup> that the link and keys were not substituted, and that the end-to-end encryption is secure.</td>
  </tr>
</table>

<sup>*</sup> Adding optional verified identities that we plan in the future will change it &mdash; the address owner will have an option to request identity verification before accepting the connection.

<sup>**</sup> Connection security code is the cryptographic hash (SHA256) of combined public keys of both sides &mdash; there are 2<sup>256</sup> possible security codes (1 with 77 zeros – about [1000 times smaller](https://www.wolframalpha.com/input?i=2%5E256) than the estimated number of atoms in the visible universe).

## Conversation preferences

Tap on one of your conversations to open conversation preferences.
Here you can choose whether you wish to allow disappearing messages, deleting messages for everyone and voice messages.
