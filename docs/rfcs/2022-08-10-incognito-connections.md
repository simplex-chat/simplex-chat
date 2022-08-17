# Incognito connections

## Problems

Allow users to connect with incognito profile using the same user account without exposing main profile - either with randomly generated per connection profile / no profile, or custom profile created by user per connection. The latter option involves designing complex UI for creating profile on connection and seems to detract from UX, so we consider first two options.

## Proposal

Add incognito mode determining whether newly created connections are incognito, and a switch on connection pages affecting current connection.

Add API to turn incognito mode on/off - it is saved as part of ChatController state to allow terminal users to set it. We can save preference on mobile and set it on chat start. We can also persist it to database to carry across terminal sessions, but it seems unnecessary.

Parameterize `AddContact`, `Connect` and `ConnectSimplex` API - create connection as incognito based on incognito mode and parameter, parameter is given preference.

### Option 1 - random profile

Add nullable field `custom_user_profile_id` to `connections` table - `incognitoProfile: Maybe Profile` in `Connection` type; when connection is created as incognito on API call, random profile is created and saved to `profiles` table.

Incognito profile is created only with a display name, it can be:

- Some prefix followed by sequence of random character/digits
- Passphrase-like (2-4 random words)
- A name from a dictionary(ies)?
- One of above chosen randomly.

We could generate other parts of profile (picture?) but it's not necessary for MVP.

When user initiates connection as incognito, incognito profile is sent as part of XInfo upon receiving CONF from joining user.

### Option 2 - no profile

Add `incognito` flag to `connections` table - `incognito: Bool` in `Connection` type.

Instead of XInfo both in `Connect` API and on receiving CONF when initiating, send a message that doesn't contain profile, e.g. XOk.

When saving connection profile (`saveConnInfo`) or processing contact request on receiving XOk / other message, contact generates a random profile for the user to distinguish from other connections. He is also able to mark this connection as incognito.

### Considerations

- Don't broadcast user profile updates to contacts with whom the user has established incognito connections.
- Add indication on chat info page that connection was established as incognito, show profile name so the user knows how the contact sees him.
- While profile names generated in option 1 may be distinguishable as incognito depending on generator, technically the fact that connection was established as incognito is not explicitly leaked, which is clear with option 2.
- We could offer same random profile generator on creating profiles, which would blend users with such profile as permanent and users who chose to connect with incognito profile to an observer (i.e. the fact that user chooses to be incognito for this specific connection is no longer leaked, just that he chose to be incognito generally).
- There's a use case for custom incognito profile created by user for a given connection in case user wants to hide the fact of incognito connection (leaked by distinguishable pattern in profile name or lack of profile), but it may better be solved by multi-profile.
- Send incognito profile when accepting contact requests in incognito mode? Parameterize API and give option in dialog?

### Groups

- If host used user's incognito connection when inviting, save same field marking group as incognito in `groups` table?
- Use incognito profile in XGrpMemInfo
- Allow host to create group in incognito profile - all connections with members are created as incognito?
