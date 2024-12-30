# Evolving content moderation

## Problem

As the users and groups grow, and particularly given that we are planning to make large groups work, the abuse inevitably grows as well.

Our current approach to moderation is the following:
- receive a user complaints about the group that violates content guidelines (e.g., most users are concerned with relatively rare cases of CSAM distribution). This complaint contains the link to join the group, so it is a public group that anybody can join, and there is no expectation of privacy of communications in this group.
- our automatic bot joins this group and validates the complaint.
- if the complaint is valid, and the link is hosted on one of the preconfigured servers, then we can disable the link to join the group.
- in addition to that, the bot automatically deletes all files sent to the group, in case they are uploaded to our servers, via server control port.

The problem of CSAM distribution is very small, compared with the network size, but without moderation it would certainly grow, and we need to be ahead of this problem, so this solution was in place for about a year - we wrote about it on social media.

The limitation of this approach is that nothing prevents users who created such group to create a new one, and communicate the link to the new group to the existing members so they can migrate there. While this whack-a-mole game has been working so far, it will not be sustainable once we add support for large groups, so we need to be ahead of this problem again, and implement a more efficient solution.

At the same time, the advantage of both this solution and of the proposed one is that it achieves removal of CSAM without compromising privacy in any way - 99% of CSAM distribution in all communication networks happens in publicly accessible groups, so while server operators cannot access it, as users, anybody can access it, and we, acting as users can use available information to remove this content without any compromise to privacy in security.

It is covered in our Privacy Policy.

## Solution

The solution to prevent further CSAM distribution by the users who do it requires restricting their activity on the client side, and also preventing migration of blocked group to another group.

Traditionally, communication networks have some form of identification on the server side, and that allows to block offending users.

Innovative SimpleX network design removed the need for persistent user identification of users, and many users see it as an unsolvable dilemma - if we cannot identify the users, then we cannot restrict their actions.

But it is not true. In the same way we already impose restriction on the sent file size, limiting it to 1gb only on the client-side, we can restrict any user actions on the client side, without having any form of user identification, and without knowing how many users were blocked - we would only know how many blocking actions we applied, but we would not have any information about whether they were applied to one or to many users, in the same way as we don't know whether multiple messaging queues are controlled by one or by multiple users.

The usual counter-argument is that this can be easily circumvented, because the code is open-source, and the users can modify it, so this approach won't work. While this argument premise is correct, the conclusion that this solution won't be effective is incorrect for two reasons:
- most users are either unable or unwilling to invest time into modifying code. This fact alone will make this solution effective in absolute majority of cases.
- any restriction on communication can be applied both on sending and on receiving client, without the need to identify this client. We already do it with 1gb file restriction - e.g., even if file sender modifies their client to allow sending larger files, most of the recipients won't be able to receive this file anyway, as their clients also restricts the size of file that can be received to 1gb.

For the group that is blocked to continue functioning, not only message senders have to modify their clients, but also message recipients, which won't happen in the absense of ability to communicate in disabled group. Such groups will only be able to function in an isolated segment of the network, with different clients and with self-hosted servers, which is outside of our zone of any moral and any potential legal responsibility (while we do not have any legal responsiblity for user-generated content, there are requirements we have to comply with that exist outside of legal requirements, e.g. requirements of application stores).

## Potential changes

This section is the braindump of possible changes for the future, they will not be implemented at once, and this list is neither exhaustive, as we can come up with better ideas, nor committed - some of the ideas below may never be implemented, they are only listed as technical possibilities.

Our priority is to continue being able to prevent CSAM distribution as network and groups grow, while doing what is reasonable and minimally possible, to save our costs, to avoid any disruption to the users, and to avoid the reduction in privacy and security - on the opposite, we are planning multiple privacy and security improvements in 2025.

### Mark files and group links as blocked on the server, with the relevant client action

Add additional protocol command `BLOCK` that would contain the blocking reason that will be presented to the users who try to connect to the link or to download the file. This would differentiate between "not working" scenarios, when file simply fails to download, and "blocked" scenario, and this simple measure would already reduce any prohibited usage of our servers. This change is likely to be implemented in the near future, to make them aware that we are actively moderating illegal content on the network, to educate users about how we do it without any compromise to their privacy, and to increase trust in network reliability, as currently our moderation actions are perceived as "something is broken" by affected users.

### Extend blocking records on files to include client-side restrictions, and apply them to the client who received this blocking record.

E.g., the client of the user who uploaded the file would periodically check who this file was received by (this functionality currently does not exist), and during this check the client may find out that the file was blocked. When client finds it out it may do any of the following:
- show a warning that the file violated allowed usage conditions that user agreed to.
- apply restrictions, whether temporary or permanent, to upload further files to servers of this operator only (it would be inappropriate to apply wider restrictions - thanks to a user who made this comment during the consultation). In case we decide that permanent restrictions should be applied, we could also program the ability to appeal this decision to support and lift it via unblock code - without the need to have any user identification.

The downside of this approach is that the client would have to check the file after it is uploaded, which may create additional traffic. But at the same time it would provide file delivery receipts, so overall it could be a valuable, although substantial, change.

To continue with the file, the clients of the users who attempt to receive the file after it was blocked could do one of the following, depending on the blocking record:
- see the warning that the file is blocked. If CSAM was sent in a group that is not distributing CSAM, this adds comfort and the feeling of safety.
- block image preview, in the same way we block user avatars.
- users can configure automatic deletion of messages with blocked files.
- refuse, temporarily or permanently, to receive files from this group member. Permanent restriction may be automatically lifted once the member's client presents the proof of being unblocked by server operator.

While file senders can circumvent client side restrictions applied by server operators, these measures can be effective, because the recipients would also have to circumvent them, which is much less likely to happen in a coordinated way.

The upside of this approach is that it does not compromise users' privacy in any way, and it doesn't interfere with users rights too. A user voluntarily accepted the Conditions of Use that prohibit upload of illegal content to our servers, so it is in line with the agreement for us to enforce these conditions. At the same time it would be inappropriate for us to restrict the ability to upload files to the servers of 3rd party operators that are not pre-configured in the app - only these operators should be able to restrict uploads to their servers.

It also avoids the need for any scanning of content, whether client- or server-side, that would also be an infringement on the users right to privacy under European Convention of Human Rights. It also makes it unnecessary to identify users, contrary to common belief that to restrict users one needs to identify them.

In the same way the network design allows delivering user messages without any form of user identification on the network protocol level, which is the innovation that does not exist in any other network, we can apply client-side restrictions on user activities without the need to identify a user. So if we apply blocking resulting in client-side upload/download restrictions, all we would know is how many times this restriction was applied, but not to how many users - multiple blocked files could have been all uploaded by one user or by multiple users, but this is not the knowledge that is required to restrict further abuse and terms violation to our servers. Again, this is an innovative approach to moderation that is not present in any of the networks, that allows us both to remain in compliance with the contractual obligations (e.g., with application store owners) and any potential legal obligation (our legal opinion is that we do not have obligation to moderate content, as we are not providing services) once it becomes a bigger issue.

### Extend blocking records on links to include client-side restrictions, and apply them to the clients who received this blocking record.

Similarly to files, once the link to join the group is blocked both the owner's client and all members' clients can impose (technically) any of the following restrictions.

For the owner:
- restrict, temporarily or permanently, ability to create public groups on the servers of the operator (or group of operators, in case of pre-configured operators) who applied this blocking record.
- restrict, temporarily or permanently, ability to upload files to operator's servers.
- restrict, temporarily or permanently, sending any messages to operator's servers, not only in the blocked group.

For all group members:
- restrict, temporarily or permanently, ability to send and receive messages in the blocked group.

For the same reason as with files, this measure will be an effective deterrence, even though the code is open-source.

While full blocking may be seen as draconian, for the people who repeatedly violate the conditions of use, ignoring temporary or limited restrictions, it may be appropriate. The tracking of repeat violations of conditions also does not require any user identification and can be done fully on the client side, with sufficient effeciency.

### Implement ability to submit reports to group owners and moderators

This is covered under a [separate RFC](./2024-12-28-reports.md) and is currently in progress. This would improve the ability of group owners to moderate their groups, and would also improve our ability to moderate all listed groups, both manually and automatically, as Directory Service has moderation rights.

### Implement ability to submit reports to 3rd party server operators

While users already can send reports to ourselves via the app, this requires additional steps. Submitting reports to 3rd party operators is even harder, as it requires going to their server pages and send messages there.

This change, if implemented, would allow sending reports to any server operator directly via the app, to the address sent by the server during the initial connection.

Server operators may be then offered efficient interfaces in the clients to manage these complaints and to apply client-side restrictions to the users who violate the conditions.

### Blacklist servers who refuse to remove CSAM from receiving any traffic from our servers

We cannot and should not enforce that 3rd party server operators remove CSAM from their servers. But we can, technically, implement blocklists of servers so that the users who need to send messages to these servers would not be able to do that via our servers.

## Actual planned changes

To summarize, the changes that are planned in the near future:

- client-side notifications that files or group links were blocked (as opposed to show error, creating an impression that something is not working).
- [content reports](./2024-12-28-reports.md) to group owners and moderators.

We will continue moderating, and as long as CSAM distribution is prevented, we may not need additional measures listed here.

At the same time, we are committed to make it impossible to distribute CSAM in the part of SimpleX network that we or any other pre-configured operators operate.
