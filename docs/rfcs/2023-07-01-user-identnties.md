# User identities

## Problem

Having user identities as the required attribute of a communication network user is suboptimal for many reasons explained elsewhere, e.g. in the recent talk at MoneroKon.

At the same time, some network services may require profile-specific identity or even device-specific identity. The scenarios when it can be helpful and necessary are:

- paid services provided anonymously. E.g., our recent survey shows that there is an interest for anonymous cloud storage, but it cannot be provided without limitations and for large storage quotas - without payments. Given that creating a user profile has no friction in the app, provision of such service for free would require a device-wide identity created specifically for this service, so it won't be used or shared for any other service, that would prevent abuse, at least on the level of client restrictions.
- surveys. For survey integrity this is important that each person participates once, so having a device-wide identity would also prevent participating multiple times, at least for most users who don't want to spend time creating app forks.

## Solution

As long as there is a protocol of requesting such per-service identity that can only be created once per app installation that is transparent to the users and requires explicit confirmation via UI, it does not seem to present an abuse of user privacy - on the opposite, it would facilitate the creation of fair and persistent relationship between the user and the service.

From the user experience point of view, network services are automatic contacts/chat bots. While for some specialized services a custom UI can be created, we will probably go in the direction of allowing services to communicate this custom UI via the service protocol rather than bundling UIs for all services in the app (which is somewhat close to the approach of Telegram). This is not the subject here though.

Initally, the connection with the service would be presented as a usual conversation, possibly with the indication that this contact is chat bot/service account (it can be just an indication in the contact profile that cannot be set via the UI, but can be set by chat bots). If the chatbot/service requires device-wide persistent identity for the user to identify to the service (or per-profile identity if the user agrees to create an additional paid account), the service would send a special chat protocol message that would interactively present as "request for identity" and to proceed with using the service, the user will have to interact with it by either sharing a device-wide identity for this service or by arranging a payment to create a new identity - these are all very preliminary ideas about how it all could work, not a design.

Client UI would preclude creating multiple per-service identity, and once the per-service identity is created the only way to remove it would be to reinstall the app with a clean database.

## Open questions

Should there be some solution to prevent technically advanced users from changing/removing these identities by manipulating database and/or files in chat archive? Are there viable approaches here?

Should this identity request chat item be custom-made (e.g. as group invitation) or should we implement UI widget framework, at least a very basic one first, and implement these requests via this framework. In this case there may be some combined approach, as this identity has deep integration with the app that is not needed for other UI widgets.
