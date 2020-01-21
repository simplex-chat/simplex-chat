# Edge-messagng protocol implementation

This document defines specific elements to be used by client and server implementations of edge-messaging protocol. This protocol relies on the connection creation and messaging flows defined in generic [edge-messaging protocol][1].

This document defines:
- [cryptographic algorithms](#cryptographic-algorithms) to sign/verify requests and to encrypt/decrypt messages.
- required approaches to generate:
   - [connection IDs](#connection-id) for clients.
   - [connection URIs](#connection-uris) for servers.
- [privacy requirements](#privacy-requirements) to the servers.
- [REST API](#rest-api):
   - to create connections and to update connection attributes.
   - to send and to retrieve messages.
- [WebSockets API](#websockets-api) to subscribe to connections:
   - to receive the new messages.
   - to update connection attributes.
- any other requirements for edge-messaging servers


## Cryptographic algorithms

Edge-messaging clients need to cryptographically sign requests:
- with the recipient's key `RK` (server to verify):
   - to create connections.
   - to subscribe to connection.
   - to change connection attributes.
   - to delete the connection.
   - to retrieve messages from the connection 
- with the sender's key `SK`:
   - to send the initial request including public key `SK` (connection recipient to verify).
   - to send messages (server to verify).

To sign and verify requests, clients and servers MUST use RSA-PSS algorythm defined in [RFC3447][2].

To optinally sign and verify messages, clients SHOULD use RSA-PSS algorythm.

To encrypt and decrypt messages, clients SHOULD use RSA-OAEP algorythm defined in [RFC3447][2].

The reasons to support these algorithms:
- they are supported by WebCrypto API.
- they are newer versions than RSA-PKCS1-v1_5 encryption and signature schemes.
- they are more widely supported than ECC algorithms

Future versions of the protocol may allow different algorithms.


## Connection ID

Edge-messaging clients MUST generate random unique ID for each new unidirectional connection.

It is not required that this ID is globally unique across all clients and servers, and this ID is known only to the server on which connection is created and to the connection recipient.

Clients MUST use cryptographically strong pseudo-random number generator to generate 64(?)-bit connection IDs.

All requests (other than Create connection) require that `connectionID` property is passed to the server in the request body. This connection ID MUST match the client-generated `connectionID` earlier passed in the request to create the connection.

If connection IDs do not match, the server MUST reject the request with HTTP status code 404 (Not Found).


## Connection URIs

Edge-messaging servers MUST generate 2 different URIs for each new connection - for recipient (that created the connection) and for sender. It is REQUIRED that:
- these URIs are different.
- they do not contain client-generated connection ID, any keys, or key hashes.
- based on 64(?)-bit number generated with cryptographically strong pseudo-random number generator.

Coonection URIs can be:
- server domain, path used to create connection and random string (e.g. `https://example.com/connection/aZ...9f), e.g. base-64 encoded random number.
- any unique URI that server recognises.


## Privacy requirements

Edge-messaging server implementations MUST NOT:
- create any logs of the client requests in the production environment.
- create any history of deleted connections or retrieved (and removed) messages.
- create any history of connection updates and store old keys or URIs.
- create any snapshots of the database they use to store connections and messages (instead edge-messaging clients must manage redundancy by using more than one edge-messaging server, e.g., as described in [graph-chat protocol][3]).
- create/store any other information that may undermine privacy or [forward secrecy][4] of communication between clients using edge-messaging server.


## REST API

### General API considerations

Edge-messaging server MUST provide REST API via HTTPS protocol. It MAY operate on the same domain as any other web application. It is RECOMMENDED that the endpoint to create connections and all connection URIs start from the same path, to avoid namespace conflicts with other applications.

In case of any requests sent to unknown URIs, server MUST reject the request with HTTP status code 404 (Not Found).

In case of any requests sent with missing required properties, incorrect property type/value or any additional unknown property (or sub-property), server MUST reject the request with HTTP status code 400 (Bad Request).

Below examples of API endpoints use:
- server `https://example.com`
- path `/connection`


### Request headers

All server requests MUST use JSON object as request body and MUST use HTTP header `Content-Type: application/json`.

... client protocol version

TODO


### Request authorisation

All server requests MUST be signed with the relevant key and the digital signature MUST be passed in HTTP header `Authorization`.

In case of signature verification failure, server MUST reject the request with HTTP status code 401 (Unauthorised).

TODO Authorisation header format


### Response headers

... server protocol version

... server timestamp

TODO


### REST API endpoints

Edge-messaging server MUST provide the API endpoints for the recipient and for the sender. The list of endpoints below has URI examples, the actual API URI schemes can differ between implementations, and even from deployment to deployment, based on server configuration.

URI scheme provides an additional layer of security to access the connection for both the sender and the recipient, and allows, if required, to implement and deploy private and commercial edge-messaging servers.

`messages` path segment in all endpoints to retrieve, delete and send messages is REQUIRED and MUST NOT be changed by any implementation or deployment.

Endpoints for the recipient:
- [Create connection](#create-connection): POST `create URI` (e.g. `https://example.com/connection`)
- [Update connection](#update-connection): PUT `<RU>` (e.g. `https://example.com/connection/aZ9f`)
- [Delete connection](#delete-connection): DELETE `<RU>` (e.g. `https://example.com/connection/aZ9f`)
- [Retrieve messages](#retrieve-messages): POST `<RU>/messages` (e.g. `https://example.com/connection/aZ9f/messages`)
- [Delete messages](#delete-messages): DELETE `<RU>/messages` (e.g. `https://example.com/connection/aZ9f/messages`)

Endpoints for the sender:
- [Update connection](#update-connection): PUT `<SU>` (e.g. `https://example.com/connection/bY1h`)
- [Send messages](#send-messages): POST `<SU>/messages` (e.g. `https://example.com/connection/bY1h/messages`)

__Please note__: the server MUST NOT allow the sender to delete of modify the messages after they are sent.


### REST API endpoints for the connection recipients

#### Create connection

URI: as defined by server configuration, can be different per server user.

Example: POST `https://example.com/connection`

Server MUST define a single endpoint to create connections. This endpoint can be:
- server domain without any path, if the domain is not shared with other web application.
- server domain and path used for all connection URIs.
- server domain, path and secure token(s) (possibly user-specific), if the server owner wants to restrict access to creating connections (for private or commercial servers).
- any other, potentially undiscoverable, URI that server recognises.

To create a connection, edge-messaging client MUST send POST request to this endpoint, signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `connectionID` (string): new client-generated connection ID.
- `recipient` (string): public key `RK` to verify digital signature of the recipient.
- `sender` (string, optional): public key `SK` to verify digital signature of the sender (it can be used in the alternative flow of establishing the connection when recipient and sender exchanged two secure out-of-band messages with each other, to reduce the number of connection steps - see [edge-messaging protocol][1]).
- `disabled` (boolean, optional): if `true`, the connection will be created but it will not be possible for the sender to use it to send messages. It will still be possible to retrieve the available messages and to modify the connection. 

Servers MUST require that connection ID is unique, and in the unlikely case of ID collision reject the connection creation request with HTTP status code 409 (Conflict).

If the connection creation succeeded, the server MUST respond with HTTP status code 201 (Created) and the response body MUST be a JSON object with the following properties:
- `recipientURI` (string): recipient URI `RU` of the connection that MUST be used as the endpoint for requests to retrieve the messages, to update connection attributes and to delete the connection. Clients MUST NOT share this URI with the sender.
- `senderURI` (string): sender URI `SU` of the connection that MUST be used as the endpoint for requests to send the messages.


#### Update connection

URI: recipient connection URI `<RU>`

Example: PUT `https://example.com/connection/aZ9f`

To update the connection, edge-messaging client MUST send PUT request to the recipient connection URI `RU` (returned by the server when creating the connection), signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `connectionID` (string): existing connection ID (see [Connection ID](#connection-id)).
- `recipient` (string, optional): public key `RK` to verify digital signature of the recipient, if the recipient wants to change the key (TBC - the request should be signed with both current and new key).
- `sender` (string, optional/required): public key `SK` to verify digital signature of the sender. Unless this key was set when connection was created, this key is required on the first connection update request.
- `newRecipientURI` (boolean, optional): if `true`, the server will generate a new URI `RU` for the recipient to use the connection.
- `newSenderURI` (boolean, optional): if `true`, the server will generate a new URI `RU` for the sender to use the connection - it has to be passed to the sender so they can continue using the connection.
- `disabled` (boolean, optional): if `true`, the connection will be "disabled" and it will not be possible for the sender to use it to send messages. It will still be possible to retrieve the available messages and for both sides to modify the connection. This parameter can be used to allow the back-pressure to the message sender (e.g. if the recipient is overloaded with message processing and cannot accept any new messages).


Server MUST permanently update required connection keys and URIs without preserving any copy.

If the connection update succeeded, the server MUST respond with HTTP status code 200 (OK) with body (possibly empty) that may have the following properties:
-  `recipientURI` (string, optional): only returned if the new recipient URI was requested to be generated with `"newRecipientURI": true`.
-  `senderURI` (string, optional): only returned if the new sender URI was requested to be generated with `"newSenderURI": true`.

If any of the connection keys have changed, all the following requests signed with the old keys MUST be rejected with HTTP status code 401 (Unauthorised).

If any of the connection URIs have changed, all the following requests to the old URIs MUST be rejected with HTTP status code 404 (Not Found).


#### Delete connection

URI: recipient connection URI `<RU>`

Example: DELETE `https://example.com/connection/aZ9f`

To delete the connection, edge-messaging client MUST send DELETE request to the recipient connection URI `RU` (returned by the server when creating the connection), signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `connectionID` (string): existing connection ID (see [Connection ID](#connection-id)).

If the connection deletion succeeded, the server MUST respond with HTTP status code 200 (OK) without body. 

Server MUST permanently delete the connection and all unretrieved messages without preserving any copy of the connection or messages.

All further requests to the recipient and sender connection URIs MUST be rejected with HTTP status code 404 (Not Found).


#### Retrieve messages

URI: `<RU>/messages`

Example: POST `https://example.com/connection/aZ9f/messages`

To retrieve messages from the connection, edge-messaging client MUST send POST request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` (it MUST NOT be changed by any implementation or deployment), signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `connectionID` (string): existing connection ID (see [Connection ID](#connection-id)).
- `pageSize` (number, optional): if set, the server will return the number of messages, from the earliest available, up to the maximum of this parameter and `PAGE_SIZE` configured in the server. If not set the server will return up to `PAGE_SIZE` available messages.
- `fromMessageID` (string, optional): if set, the server will retrieve the messages received starting from the message with server message ID (unique per server) passed in this parameter. This ID of the next available message is passed in the response to this request (if more messages are available).
- `keepMessages` (boolean, optional):  if `true`, the server will keep the retrieved messages available in the connection to be retrieved again (or deleted via a separate request). By default the retrieved messages will be removed from the server. Clients may need to process messages in some way, and until the processing succeded clients may choose to keep messages on the server to ensure they are not lost if processing fails for any reason.

Edge-messaging server MUST permanently remove the retrieved messages, unless specifically instructed by the clients to keep them.

__Please note__: server implementations MUST NOT track in any form how many times or whether the messages were retrieved.

If the unknown message ID is passed in `afterMessageID` parameter, the request should be rejected with HTTP status code 400 (Bad Request).

If the request is successful, the server MUST respond with HTTP status code (200) returning as response body the required number (but not more than `PAGE_SIZE` configured in the server) of the earliest sent messages with the following properties:
- `messages` (array): retrieved messages. Each retrieved message is an object with the following properties:
   - `id`: server-generated unique ID allowing to identify messages until (they are deleted from the server) and to paginate responses.
   - `ts`: server timestamp of the time when the message was received from the sender.
   - `msg`: encrypted message body, that the recipient should be able to decrypt with the key `EK`.
- `nextMessageID` (string, optional): if server has more messages available it MUST return this parameter that can be used by the next request in `fromMessageID` property.


#### Delete messages

URI: `<RU>/messages`

Example: DELETE `https://example.com/connection/aZ9f/messages`

To delete messages from the connection, edge-messaging client MUST send DELETE request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` (it MUST NOT be changed by any implementation or deployment), signed with the key `RK`.

This request SHOULD be used by edge-messaging clients to delete the previously retrived messages when `"keepMessages": true` parameter was used or in case they no longer require to retrive the messages.

Request body should be sent as JSON object with the following properties:
- `connectionID` (string): existing connection ID (see [Connection ID](#connection-id)).
- `pageSize` (number, optional): if set, the server will delete up to the requested number of messages, otherwise all messages, in both cases from the message ID in `fromMessageID` parameter (or from the earliest available).
- `fromMessageID` (string, optional): the server will delete the messages received, starting from the message with the server message ID passed in this parameter. If not specified, it defaults to the server message ID of the earliest received message.

Edge-messaging server MUST permanently remove the messages as requested.

If the request is successful, the server MUST respond with HTTP status code (200) with the body that has the count of deleted messages in `deleted` property.

If the unknown message ID is passed in `fromMessageID` parameter, the request should be rejected with HTTP status code 400 (Bad Request).


### REST API endpoints for the connection sender

#### Update connection

URI: sender connection URI `<SU>`

Example: PUT `https://example.com/connection/bY1h`

To update the connection, edge-messaging client of the sender MUST send PUT request to the sender connection URI `SU` (returned by the server to the connection recipient when creating the connection), signed with the key `SK`.

Request body should be sent as JSON object with the following properties:
- `sender` (string, optional): the new public key `SK` to verify digital signature of the sender. This parameter is only allowed if the sender key `SK` is already available on the connection, otherwise the server MUST reject the request with HTTP status code 401 (Unauthorised).
- `newSenderURI` (boolean, optional): if `true`, the server will generate a new URI `SU` for the sender to use the connection.
- `recipient`, `newRecipientURI`, `disabled`: these parameters are prohibited, and if any of them is present the server MUST reject the request with HTTP status code 401 (Unauthorised).

Server MUST permanently update required connection keys and URIs without preserving any copy.

If the connection update succeeded, the server MUST respond with HTTP status code 200 (OK) with body (possibly empty) that may have the following properties:
-  `senderURI` (string, optional): only returned if the new sender URI was requested to be generated with `"newSenderURI": true`.

If the connection key `SK` has changed, all the following requests signed with the old key MUST be rejected with HTTP status code 401 (Unauthorised).

If the connection URI `SU` has changed, all the following requests to the old URI MUST be rejected with HTTP status code 404 (Not Found).


#### Send messages

URI: `<SU>/messages`

Example: POST `https://example.com/connection/bY1h/messages`

To send messages to the connection, edge-messaging client MUST send POST request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` (it MUST NOT be changed by any implementation or deployment), signed with the key `SK`.

Request body should be sent as JSON object with the following properties:
- `messages` (array): retrieved messages. Each sent message is an object with the following properties:
   - `msg`: encrypted message body, that the recipient should be able to decrypt with the key `EK`. Any message meta-data (client timestamp, ID, etc.) MUST be inside the encrypted message and MUST NOT passed via additional properties.

If the request is successful, the server MUST respond with HTTP status code 200 (OK) without body.


## WebSockets API

TODO

**Simplex connection operation:**

![Simplex connection operations](/diagrams/simplex2.svg)

Sequence diagram does not show E2EE - connection itself knows nothing about encryption between sender and receiver.


[1]: edge-messaging.md
[2]: https://tools.ietf.org/html/rfc3447
[3]: graph-chat.md
[4]: https://en.wikipedia.org/wiki/Forward_secrecy
