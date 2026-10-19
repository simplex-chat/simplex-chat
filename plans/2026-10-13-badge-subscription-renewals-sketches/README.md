# Badge subscription renewals: diagrams

These are diagrams of the badge subscriptions code as built in PR #7716.

![Who holds what](components.svg)

What the apps, chat core, the badge service and the two stores each hold, and who talks to whom; core never calls a store.

![Buying a subscription on iOS](buy-ios.svg)

Buying on iOS: the app sends Apple's signed transaction, and the service checks it offline, then asks the App Store Server API by original transaction id.

![Buying a subscription on Android](buy-android.svg)

Buying on Android: the app sends the product id and purchase token, and the service reads the subscription from Play and acknowledges it itself, just before crediting.

![The service's renewal check](renewal-check.svg)

The renewal check: once paid coverage ends, the service asks the store at most once an hour per subscription and grants a new charge in one transaction, and the app acts on each kind of answer.

![Subscription states and what each shows](subscription-states.svg)

Subscription states: the store statuses behind each of the four client states, and what each shows on Your badge and in the chat-list banner.

![The retirement window](retirement-window.svg)

The retirement window: the two columns, the 67-day bound, a fresh run after a long gap, a renewal clearing the run, and a service that never replies keeping the badge.

![Request lanes and the store limits on each](request-lanes.svg)

Request lanes: which requests go to the main, purchase and renewal lanes, and the store limits on each.
