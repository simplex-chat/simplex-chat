---
title: Making connections
---

# Making connections

Because you have no user ID on the SimpleX platform, nobody can contact you unless you share a one-time invitation or a SimpleX address. Tap or click on the following sections to learn more:

- [One-time invitation](#one-time-invitation)
- [SimpleX address](#simplex-address)
- [Connecting via link or QR code](#connect-via-link-or-qr-code)
- [One-time invitation vs. SimpleX address](#one-time-invitation-vs-simplex-address)

## One-time invitation

As the name implies, it can only be used once to connect with someone. You can create and share them with your contacts as a link from a distance or as a QR code to be scanned in person or via a video call. 

#### To create and share a one-time invitation with your contact:

1. On Android, tap on the 🖉 (floating pencil) button in the bottom-right corner of the **Chats** screen. On iOS, tap on the 🖉 (pencil) button in the top-right corner of the **Chats** screen.
2. Tap or click **Add contact**.
3. Tap or click **Share this 1-time link** to share it with your contact from a distance. Or, you can show your QR code in person for your friend to scan.
4. (Optional) Toggle **Incognito** on to share an incognito profile with your contact.
5. (Optional) Return to the **Chats** screen and then click or tap on the invitation to set a contact name to help remember who it's for.

## SimpleX address

You can create a SimpleX address for other people to connect with you. When people connect to you via this address, you will receive a connection request that you can accept or reject. You can configure an automatic acceptance of connection requests with a welcome message that will be sent to new contacts when they connect with you. You can also share your SimpleX address on your chat profile, so group members can connect with you and your contacts can share it with others, if this is something you want.

#### To create and share a SimpleX address:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Tap or click **Your SimpleX address**.
3. Tap or click **Create SimpleX address**. 
4. (Optional) Tap or click **Share** to share your SimpleX address with your contacts.
5. (Optional) Publish your SimpleX address on your personal website, email signature or anywhere else for people to see. 
6. (Optional) Toggle **Auto-accept** to automatically accept connection requests. 
7. (Optional) Toggle **Accept incognito** to share incognito profiles. 
8. (Optional) Add a welcome message. 

**Please note**: if you start receiving too many requests via your SimpleX address, it's safe to delete it from your chat profile – all the connections you've made with this address will remain active, as it's not used to deliver messages.

## Connect via link or QR code

You can securely connect with other users via link or QR code. 

#### If you don't have SimpleX Chat installed on your device:

1. Tap or click on the SimpleX link you received from your contact. You'll be redirected to a webpage detailing instructions on how to connect. 
2. After you've download and installed SimpleX Chat on your device, tap **Open in mobile app** on the webpage to connect with your contact.

#### If you have SimpleX Chat installed on your device:

- Tap or click on the QR code symbol in the right-hand side of the search bar on the **Chats** screen. Scan QR code or paste link you received from your contact. You'll be prompted to share your current chat profile or an incognito profile to connect with them.
- Copy the SimpleX Link you received from your contact. Paste it into the searchbar towards the top of the **Chats** screen. You'll be prompted to share your current chat profile or an incognito profile to connect with them.
- Tap or click on the SimpleX link you received from them. You'll be prompted to share your current chat profile or an incognito profile with them.

## One-time invitation vs. SimpleX address

<table>
  <tr>
    <th></th>
    <th>One-time invitation</th>
    <th>SimpleX address</th>
  </tr>
  <tr>
    <td>Can be used many times?</td>
    <td>No</td>
    <td>Yes</td>
  </tr>
  <tr>
    <td>Can be included in user profile?</td>
    <td>No, as it can only be used once.</td>
    <td>Yes, to allow group members to connect with you directly, and your contacts to pass it on to their contacts.</td>
  </tr>
  <tr>
    <td>When to use it?</td>
    <td>With somebody you know, via another communication channel or QR code (in person or during a video call)</td>
    <td>Where many people can see and connect via it, e.g. in email signature, website, social media or group chat.</td>
  </tr>
  <tr>
    <td>Security</td>
    <td>More secure, as it can only be used once, and the initial connection request (including profile) is encrypted with double ratchet.</td>
    <td>Initial connection request is also e2e encrypted, but without double ratchet (it is initialized when request is accepted).</td>
  </tr>
  <tr>
    <td>Identification</td>
    <td>Both sides know who they connect to, as they know with whom and by who the link was shared. You can add a contact name to this invitation as soon as you share it or use it, to identify the other person when connection is established.</td>
    <td>Only the person using the address knows who they connect to, via the channel where they found the address (email, social media, etc.). The address owner can only see the user profile of the request, and has no proof of identity from the person sending the request<sup>*</sup>.</td>
  </tr>
  <tr>
    <td>Advantages over other platforms</td>
    <td>There is no direct analogy. Other platforms don’t offer one-time invitations without any fixed part identifying the user.</td>
    <td>Unlike addresses in other platforms, SimpleX addresses are not used to deliver the messages — only the initial connection requests.<br>It means that removing this address will not break the contacts made via it (like changing an email address would), it would only prevent new connections, which makes it a good solution against spam and abuse.</td>
  </tr>
  <tr>
    <td>Vulnerability to attacks</td>
    <td>Until the connection is established, anybody who intercepts this link can connect to it, so it has to be verified with the original contact that the connection succeeded.</td>
    <td>These addresses are vulnerable to connection request spam. Unlike other platforms, you can delete or change the address, without losing any contacts (see above).</td>
  </tr>
  <tr>
    <td>Passive attacks on connection links</td>
    <td colspan="2">Both types of links are not vulnerable if simply observed — they only contain public keys. So they can be safely shared via insecure or public channels, as long as you can confirm that you connected to the intended person.</td>
  </tr>
  <tr>
    <td>Active attacks on connection links</td>
    <td colspan="2">If the link is substituted via the attack on the channel used to share it, the connection security can be compromised, and the original messages monitored (man-in-the-middle attack). If it is a real risk then security code should be verified to mitigate it - doing so proves<sup>**</sup> that the link and keys were not substituted, and that the end-to-end encryption is secure.</td>
  </tr>
</table>

<sup>*</sup> Adding optional verified identities that we plan in the future will change it &mdash; the SimpleX address owner will have an option to request identity verification before accepting the connection request.

<sup>**</sup> Connection security code is the cryptographic hash (SHA256) of combined public keys of both sides &mdash; there are 2<sup>256</sup> possible security codes (1 with 77 zeros – about [1000 times smaller](https://www.wolframalpha.com/input?i=2%5E256) than the estimated number of atoms in the visible universe).
