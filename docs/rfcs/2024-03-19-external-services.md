# Accessing services provided by other apps

## Problem

SimpleX Chat can provide messaging functionality to other apps. While one of the possible approaches is to use Share extension (see below), it can only be used to send messages to the existing conversations, and there can be a need for a more general approach to establish connections with the new contacts from other apps. E.g., it could be SimpleX address discovery via known cryptocurrency blockchains addresses - to connect to and to send messages to payment contacts.

SimpleX Chat can consume functionalities from other apps. Payments is the most interesting case where it would be interesting, and it is frequently requested by the users. While simple integration via URIs is possible, in many cases consuming functionalities of other apps may require some interactive rpc protocol that is not possible in this way.

Depending on the platform this integration can be more or less complex. Desktop and Android platforms are simple - apps can expose services via the local TCP ports (e.g., with WebSockets encoding) and via Android services, - iOS platform is more complex as there is no generally usable service layer for iOS platform and there are various restrictions to background execution that need to be taken into account.

## Possible technologies for iOS platform

Most likely, not a single framework or approach but a combination of available frameworks should be used.

The examples consider the scenarios when SimpleX Chat is a consumer, and another app is a provider of the payment services.

### Custom URL schemes

This approach is the simplest to open some page or perform some action in the provider app. The downside of this approach is that if it's used on its own, then it may result in opening any other app, as any app can declare this custom scheme.

Some other approach should be added to determine if the provider app is installed, for example App groups - see below. While security considerations can be addressed with the universal links, they have privacy downside, as they will result in undesirable Internet access in case the target app is not installed.

For example, if the user wants to see the list of all transactions with the contact or any other detailed information about the payments, it may be better to use URLs to open it in the payment provider's UI, rather than to show it in SimpleX Chat UI.

### Share extension

This extension can be used when it is required to load some information from the provider app and provide some custom interface to perform some action.

While it's commonly used to share content and to send messages, it can also be used to send the payments or request some other actions, when custom provider UI is appropriate and interaction is mostly one way.

### App groups

App group is a shared container that can be accessed by multiple apps or multiple parts on one app. Traditionally it is used for sharing data with the app extensions (e.g., Share or Notification service extensions) and for sharing data between the multiple apps of the same developers. It can also be used by sharing the information via files and also communicating between two different running processes via file coordinator notifications.

While this approach can be used for sharing files and communication between active processes, it has these limitations:
- the name of the app group requires some management, as it has to be common for two apps.
- the suspended app will not receive any notifications.

### File provider extension

This extension allows sharing files and access to folders with other applications and processes. This extension can wake to process file access and change requests, and also can be used to execute arbitrary XPC service calls associated with files, so unlike app groups it will allow the app to process service requests from suspended state.

File provider extension has these limitations:
- it has to run within 25 mb memory (other extensions have the same limit).
- it is a separate process, and unless the provider app already uses shared files and shared database it may require some re-engineering to move app data to shared container of the app's app group.
- it has to use files, as while it's possible to use a "fake file" simply to create a service, it would not be compliant with app store policies.

The advantage of File provider extension over App group is that the provider app can have a very granular control of the permissions, only allowing the consumer app to read some files, and submit new files only to specified locations, and not have access or make any service requests to the locations of other consumer apps.

We made a working proof of concept to move shared logic of SimpleX Chat core into a File provider extension so that both the app and Notification service extension could use the same database and network access. That used "fake file" though, and we didn't manage to make it work withing 25mb - for full chat core it's really hard to fit in this memory limit. Some subset of functions can certainly fit in this memory limit, both for SimpleX Chat, if it's a provider, and for payment provider app.

You can see this POC here: https://github.com/simplex-chat/simplex-chat/tree/_archived-ep/ios-file-provider/apps/ios (the folders to look at are "SimpleX Service" and "SimpleXServiceProtocol") - it all worked surprisingly well, till it hit 25mb memory limit.

The good thing about payment services is that using files to expose the service and communicate service requests and results is quite natural, as making payment orders into documents rather than API calls has its advantages for debugging etc. Also providing transaction statements as PDF or text files is also quite traditional for payment providers.

## Proposed solution for payment provider integration

The assumption here is that it is desirable to provide the most friction-less user experience, where the payment can be started and completed in SimpleX Chat, without switching back and forth between the apps, including the negotiation of one-off payment addresses, payment confirmations, confirming commissions, etc.

While this approach might be seen as reducing the "engagement" with payment provider app, at the same time it may 10x the number of payments that the users are making and also 10x the size of the payment network - via simplified automatic payment address discovery and negotiation, and payment confirmation, without the need to publish a permanent payment address on the user's profile (which most users of communication solutions do not want to do).

Out of all available technologies, the combination of these technologies is likely to provide the best user experience:
- File Provider extension - to "publish" service catalogue and to communicate payment orders and results/confirmations via the associated XPC service.
- interactive local notifications from the provider app - to notify the user about some relevant events in the payment provider app and to get user confirmations
- Custom URL scheme - to open payment provider app directly for some complex interactions, where appropriate.
 
## File provider specification for service provider apps

The service provider would "publish" a "service catalogue", as the list of pre-defined files and folders structure, as will be defined by the specification.

Service catalogue would include the file with the list of services (to allow the app to provide more than one service for the future extensibility, so "Payments" and "Messaging" each would be a separate service, provided by payments/wallet app and messenger app, respectively) and each service definition files would exist in a separate catalogue.

Catalogue for a service would contain the file with the list of "Request template" files and "Endpoint" files - request template file itself could be an end-point. This follows from the design of the XPC service of File provider extension where each request should be made in the context of some file.

While it's tempting to simply use directory as the list of such files, it makes it harder to test some new functions by adding the new files without exposing them to the consumer apps, plus having the list of endpoints and templates in a separate file allows to associate additional metadata with each one. The most human- and machine-readable format for this list and also for templates and for endpoints is YAML, a specific specification needs to be defined, e.g. using RFC 8927 spec for JSON Type definitions - as we only likely need the subset of YAML that is simply a syntactically different JSON.

A sample directory structure for service catalogue:

```
.services/
  services.yml
  payments/
    definition.yml
    invoice_request.yml
    payment_order.yml
    proof_request.yml
    statement_request.yml
    invoice_requests/
      chat.simplex.app/
        20240320110125_invoice_request.yml # a specific invoice request from SimpleX Chat
    payment_orders/
      chat.simplex.app/
        20240320110125_payment_order.yml # a specific payment order from SimpleX Chat
    proof_requests/
    statement_requests/
```

This folder structure would allow using file provider extension for exposing the files to Files app, alongside the `.services` folder (we could come up with some fancier name to make it more distinct).

`services.yml` file:

```
spec: Services
versions: 1
services:
  - spec: payments # this name should be reserved and managed as specification name
    versions: 1-2 # single supported version or version range that service provider can accommodate
    folder: payments
    status: enabled # optional, could be also "experimental", "disabled"
    countries: [!CHN] # optional, but may be required at some point to disable the service in some countries, to avoid removal from these countries App store.
```

Open question: do we want a general purpose spec for all payments, or we want to have a separate spec for crypto payments only. I think the former, but it's a trade-off.

`definition.yml` file:

```
spec: payments # optional, should match services.yml file
versions: 1-2 # optional, should match services.yml file
status: enabled # optional, should match services.yml file
countries: [!CHN] # optional, should match services.yml file
requests: # request templates
  - spec: invoice_request # spec contains standardized template name that defines file schema
    versions: 1-3 # optional, to allow independently version specific templates and endpoints
    file: invoice_request.yml # app-defined name for template
    folder: invoice_requests # folder where consumer app would submit invoices
  - spec: payment_order
    file: payment_order.yml
    folder: payment_orders
  - spec: proof_request # e.g. to validate asset ownership without making the payment
    file: proof_request.yml
    folder: proof_requests
  - spec: statement_request
    file: statement_request.yml
    folder: statement_requests
endpoints:
  - spec: connect_app
    file: connect_app.yml # some XPC request in the context of that file could perform the initial handshake to agree app signature, and optionally, encryption keys, and to provide folder for the consumer app to submit requests
```

While it's tempting to define the whole service as a single endpoint, without any request files (like we did in the POC for SimpleX Chat XPC service), it will make it very hard to justify using File provider extension in this case - Apple guidelines are very clear that the extensions should be used for their intended purposes. With request files it seems very legitimate. So possibly the initial service spec should not contain any endpoints, only request templates.

I am not a cryptocurrencies expert, so below are just examples that require changing / extending to accommodate various cryptocurrency conventions, and, possibly some currencies may require completely separate request structures. These examples are just to review this file-based request approach.

`invoice_request.yml`

```
spec: invoice_request
version: 1-3 # this could be the syntax for allowed numbers range, the consumer app should submit a specific number
currency: # this could be the syntax to define a single XPC service call to this template to obtain the list of currencies addresses
  spec: list_currencies
address: 
  - spec: addresses_for_currency # XPC service call to list existing receiving addresses and their purposes for a given currency
  - spec: new_address_for_currency # XPC call to create a new receive address
amount:
  spec: get_amount_range_for_currency
# or
# amount: 0-100
file_endpoint: # supported requests is to be made in the context of the created request file, not in the context of the template
  - spec: get_invoice_request_status
```

The idea here is that template defines the list of supported XPC service calls that should be made in the context of the file, sent as simple strings, that the consumer app would use to populate the request document. It also contains the list of service calls that can be made in the context of the created request file. These calls should be part of the list defined in the specification, so that a consumer app knows what subset of the specification is supported.

This might be a bit over-complicated, as it's just the initial brain-dump, and it can all be narrowed down to the version that would define the list of calls the provider app must support, instead of trying to re-invent the self-documenting API approach that nobody likes using.

`20240320110125_invoice_request.yml` file

```
spec: invoice_request
version: 2
currency: XMR
# address: asfeahsd
amount: 0.5
```

Possibly, there is no need for invoice requests at all, as all we really need here is the list of addresses, and, possibly, the creation of the new addresses. On another hand, the invoice can be used to query whether the payment was received, particularly for the new random 1-time addresses and address field could also be populated by payment provider app to be read by consumer app.

`20240320110125_payment_order.yml` file

```
spec: payment_order
version: 2
currency: XMR
address: asfeahsd
# maybe there should also be some field for the source of funds
amount: 0.5
```

## Other platforms

It is tempting to avoid using file-based approach on the platforms that allow simple service calls. But that would result in the need for alternative parallel service specification, and would complicate and increase the scope for development and testing. Both desktop systems and Android have similar primitives to allow file updates. The trade-off here is between having a single file-based protocol on all platforms, that would simplify testing and development, and potentially lower security of files - the latter can be mitigated by using encryption for these files that is agreed during initial handshake between provider and consumer apps.
