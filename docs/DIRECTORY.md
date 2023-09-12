---
title: SimpleX Directory Service
revision: 18.08.2023
---

# SimpleX Directory Service

You can use an experimental directory service to discover the groups created and registered by other users.

## Searching for groups

Connect to the directory service via [this address](https://simplex.chat/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion) and send the message containing the words you want to find in the group name or welcome message. You will receive up to 10 groups with the largest number of members in the response, together with the links to join these groups.

Please note that your search queries can be kept by the bot as the conversation history, but you can use incognito mode when connecting to the bot, to avoid correlation with any other communications. See [Privacy policy](../PRIVACY.md) for more details.

## Adding groups to the directory


### How to add a group

To add a group you must be its owner. Once you connect to the directory service and send `/help`, the service will guide you through the process.

1. Invite SimpleX Service Directory to the group as `admin` member. You can also set the role to `admin` after inviting the directory service.

The directory service needs to be `admin` to provide a good user experience of joining the group, as it will create a new link to join the group, which is expected to be online 99% of the time.

2. Add the link sent to you by the directory service to the group welcome message. This has to be done by the same group member who invited the directory service to the group. This member will be the owner of the group record in the directory service.

3. Once the link is added, the group will need to be approved by the directory service admins. This link is functional even before the group is approved, and you can continue using this link even if the group is not approved.

The group is usually approved within 24 hours. Please see below which groups can be added.

Once the group is approved, it will appear in search results.

You can list all the groups you submitted by sending `/list` to the directory service.

### How to remove the group from the directory

Changing the group profile in any way (e.g., changing the group name, welcome message, or removing the link to join the group from the welcome message) will remove the group from the search results until the group is approved again by the directory service admins.

If it is undesirable that the service cannot be found in search during this time, please coordinate the time of this change with the directory service admins for quick approval.

Changing the role of the directory service will temporarily remove the group from the search results, and unless you changed the role to the `owner`, it will also permanently disrupt the members that were in the process of connecting to other members via the directory service.

To remove the group from the directory:

1. Remove the group link created by the directory service from the welcome message. This will not disrupt the members from joining the group, even via this link, but will remove the group from the search results.
2. After some time (we recommend 3-4 days) remove the directory service from the group - it will stop receiving the messages and the group will be permanently removed from the search results.

Removing the group does not prevent you from registering the group again in the future.

### Why limit which groups can be added

The reason to restrict the acceptable content is to have a better experience for a wider range of the users, and to comply with the content policies of app distribution channels (App Store, Play Store, etc.), once the directory service is available via the app without additional configuration. To achieve that, the content in the listed groups should be restricted to be generally appropriate.

Doesn't it go against the idea of decentralization and freedom of speech?

We believe it does not, because:

1. The service only restricts the content in the groups that you choose to register – we have no register of all existing groups, and no access to their content.
2. The service itself is open-source, and can be self-hosted, so anybody can run an alternative directory service with the different content policies, or without any policies at all.
3. Freedom of speech should respect legal rights and freedoms of others, so agreeing some boundaries seems necessary.

### Which groups can be added

The below is not the final policy, it is a work in progress.

Currently, the group registration is limited and manual, as we have limited resources to evaluate the content of the groups, so the initial content policy is quite restrictive - we believe it is better to be able to extend what is allowed, than to have to reduce it.

To be "listed in the directory" <sup>\*</sup>, the group must have at least 10 members. Both the group and group owner profiles must include relevant, appropriate, non-offensive avatar images, that do not use the existing trademarks.

Please ONLY submit the groups on the following subjects:
- communications solutions and providers (messengers, social networks, Internet, etc.)
- privacy and security
- cryptocurrencies
- product and software development
- science and technology
- media and entertainment: books, music, movies and games
- politics, society, culture and education

The content in the group must be "appropriate" for the general audience, starting from 12 years old.

The content in the listed groups must:
- be legal for the jurisdiction you are in.
- NOT contain spam and advertising.
- NOT contain violence, calls for violence, calls for public demonstrations, or any other disturbing content.
- NOT contain pornography, nudity, erotic or any sex-related content.
- NOT contain racism, hate speech, or any other content that promotes discrimination.
- NOT contain information about drugs, alcohol, or any other substances.
- NOT contain [NSFW](https://en.wikipedia.org/wiki/Not_safe_for_work) content.
- NOT be offensive – this needs to be clarified.

Group owners are expected to moderate the content in the groups, if members post inappropriate or excessive amount of content and group owners do not moderate it, the group is likely to be removed from the directory.

We reserve the right to not accept the group listing in the directory or cancel its listing, and there may be cases when we can't provide an explanation. We will certainly try to avoid it by communicating with the group owners first.

The combination of display name and full name has to be unique for the listed groups.

Once the group is listed in the directory, the bot will invite you to join the group of the group owners, where you can send any ideas or suggestions for how the groups functionality should evolve, and help steer both the product and the policies.

<sup>\*</sup> "listed" means discoverable via search or any other directory service functions by any connected users other than the user who submitted the registration
