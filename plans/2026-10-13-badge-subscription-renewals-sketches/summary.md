# Badge subscriptions (PR #7716)

Supporter badges (supporter and legend tiers) were prepaid: a code bought on the web, or a one-month store purchase not yet offered. This PR sells them as App Store and Google Play subscriptions, monthly or annual, across the badge service, core and apps.

![Who holds what](components.svg)

## Buying

The app passes the store's receipt, through core, to the badge service, which checks it with the store and credits the badge.

- **iOS:** the service checks Apple's signature, then asks Apple about the subscription by its original transaction id, which renewals keep. An Apple ID that subscribed before can't subscribe again.
- **Android:** the service reads the subscription from Play by its purchase token and acknowledges it. Play refunds purchases left unacknowledged for three days.

![Buying on iOS](buy-ios.svg)

![Buying on Android](buy-android.svg)

## Renewals

Once the paid months run out, the app requests the next badge credential at the weekly moment all apps share, then daily, and only then does the service ask the store whether it charged again, at most hourly per subscription. A charge counts from its billing-period start. The app also reports the device's view of the store, such as auto-renewal off, for the Your badge screen and chat-list banners.

![The renewal check](renewal-check.svg)

![Subscription states](subscription-states.svg)

## How a subscription ends

While the store retries a failed payment, the badge stays, though contacts see it expire a week after its last credential, and the profile can't buy or redeem another. It is removed when the service answers that the subscription ended, or after 67 days of answers that renewed nothing: 60 days, the longest either store retries, plus 7 days of contacts' grace. If the service never replies, the badge stays.

![The retirement window](retirement-window.svg)

## Protecting the service and the stores

Purchases that ask a store, renewal checks and everything else run on separate queues, so fake purchases can delay only other purchases. Each subscription is checked at most hourly, and the purchase path makes at most 300 Play subscription checks a minute in total, a tenth of Play's default quota; a Play purchase turned away for three days is refunded.

![Request lanes](request-lanes.svg)

## Protocol and database changes

- **Badge RPC** (still version 1): an optional subscription state in credential answers; charges use the existing `charge` ledger entry.
- **Chat API:** a command reporting the store's state, a `listed` flag for receipts from the store's purchase list, a refusal while a store receipt may still be credited, and the subscription and a "subscription cancelled" alert in the badge state.
- **Databases:** three columns in core's, for the store's state and the answers that renewed nothing; on the service, a charges table, a unique charge id on ledger rows, and state, check time and Play token on payments.

## Known limits

- Crediting and renewals are tested only against fake stores built from the stores' documentation.
- A removed subscription is never checked again: if its store still charges (after a passing "ended", a Play pause over 67 days, or 67 days of service or store errors), the user pays for nothing.
- A purchase completing while another badge shows waits for it to end, and may meanwhile be refunded or lose paid periods.
- Play purchases are acknowledged before being signed and recorded; if either fails, Play won't refund, and only a later successful retry credits them.
- On Postgres, the service needs one database connection.

## Before sale

Not built: iOS resubscription, moving a subscription to a new device, tier changes (an iOS switch is charged, never granted), refunds (a refunded annual keeps its badge), and spreading the weekly moment.

Store setup: App Store Billing Grace Period and Family Sharing off (once on, Family Sharing can't be undone). Play: minimum grace period, account hold on, pausing off. Both: no offers of any kind, server API credentials, enough quota, translations, store review (Sandbox purchases are refused as test purchases, an open question), and products hidden until these apps ship.
