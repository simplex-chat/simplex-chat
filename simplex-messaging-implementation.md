# Simplex messaging protocol implementation

This document defines specific elements to be used by client and server implementations of simplex messaging protocol. This protocol relies on the connection creation and messaging flows defined in generic [simplex messaging protocol][1].

This document defines:
- [cryptographic algorithms](#cryptographic-algorithms) to sign/verify requests and to encrypt/decrypt messages.
- [connection URIs](#connection-uris) generation by the servers.
- [privacy requirements](#privacy-requirements) to the servers.
- [REST API](#rest-api):
   - to create and to secure connections.
   - to send, to retrieve and to delete messages.
- [WebSockets API](#websockets-api):
   - to subscribe to connections.
   - to receive the new messages.
- any other requirements for simplex messaging servers


## Cryptographic algorithms

Simplex messaging clients need to cryptographically sign requests:
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


## Connection URIs

Simplex messaging servers MUST generate 2 different URIs for each new connection - for recipient (that created the connection) and for sender. It is REQUIRED that:
- these URIs are different.
- based on 64-bit number generated with cryptographically strong pseudo-random number generator.

Coonection URIs can be:
- server domain, path used to create connection and random string (e.g. `https://example.com/connection/aZ...9f), e.g. base-64 encoded 64-bit random number.
- any unique URI that server recognises.


## Privacy requirements

Simplex messaging server implementations MUST NOT create, store or send to any other servers:
- logs of the client requests in the production environment.
- history of deleted connections or retrieved (and removed) messages.
- snapshots of the database they use to store connections and messages (instead simplex messaging clients must manage redundancy by using more than one simplex messaging server, e.g., as described in [graph-chat protocol][3]).
- any other information that may compromise privacy or [forward secrecy][4] of communication between clients using simplex messaging server.


## REST API

### General API considerations

Simplex messaging server MUST provide REST API via HTTPS protocol. It MAY operate on the same domain as any other web application. It is RECOMMENDED that the endpoint to create connections and all connection URIs start from the same path, to avoid namespace conflicts with other applications.

In case of any requests sent to unknown URIs, server MUST reject the request with HTTP status code 404 (Not Found).

All request parameters are required, unless specified as optional. In case of any requests sent with missing required properties, additional unknown parameters, incorrect property type/value or any additional unknown property (or sub-property), server MUST reject the request with HTTP status code 400 (Bad Request).

Request body, unless specified, MUST be empty, otherwise the server should reject the request with HTTP status code 400 (Bad Request).

Servers MUST NOT allow cookies in the requests. Any request with cookies MUST be rejected with HTTP status code 400 (Bad Request).

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

Simplex messaging server MUST provide the API endpoints for the recipient and for the sender. The list of endpoints below is exhaustive and servers MUST NOT implement any other endpoints. The actual API URI schemes can differ between implementations from the below examples, and between deployments, based on server configuration.

URI scheme provides an additional layer of security to access the connection for both the sender and the recipient, and allows to implement and deploy private simplex messaging servers.

`/messages` path segment is REQUIRED in all endpoints to retrieve, delete and send messages and MUST NOT be changed by any implementation or deployment.

Endpoints for the recipient:
- [Create connection](#create-connection): POST `create URI` (e.g. `https://example.com/connection`)
- [Secure connection](#secure-connection): PUT `<RU>` (e.g. `https://example.com/connection/aZ9f`)
- [Delete connection](#delete-connection): DELETE `<RU>` (e.g. `https://example.com/connection/aZ9f`)
- [Retrieve messages](#retrieve-messages): GET `<RU>/messages[?fromMessageId=<messageId>]` (e.g. `https://example.com/connection/aZ9f/messages`)
- [Retrieve message](#retrieve-message): GET `<RU>/messages/<messageId>` (e.g. `https://example.com/connection/aZ9f/messages/1234`)
- [Delete message](#delete-message): DELETE `<RU>/messages/<messageId>` (e.g. `https://example.com/connection/aZ9f/messages/1234`)

Endpoints for the sender:
- [Send message](#send-message): POST `<SU>/messages` (e.g. `https://example.com/connection/bY1h/messages`)

__Please note__: the server MUST NOT allow the sender to delete or modify the messages after they are sent via any endpoints or any other means.


### REST API endpoints for the connection recipients

#### Create connection

POST: `CREATE_CONNECTION` URI as defined by server configuration, can be different per server user.

Example: POST `https://example.com/connection`

Server MUST define a single endpoint to create connections. This endpoint can be:
- server domain without any path, if the domain is not shared with other web application.
- server domain and path used for all connection URIs.
- server domain, path and secure token(s) (possibly user-specific), if the server owner wants to restrict access to creating connections (for private or commercial servers).
- any other, potentially undiscoverable, URI that server recognises.

To create a connection, simplex messaging client MUST send POST request to this endpoint. The request MUST be signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `recipient` (string): public key `RK` to verify digital signature of the recipient.

If the connection creation succeeded, the server MUST respond with HTTP status code 201 (Created) and the response body MUST be a JSON object with the following properties:
- `recipientURI` (string): recipient URI `RU` of the connection that MUST be used as the endpoint for requests to retrieve the messages, to update connection attributes and to delete the connection. Clients MUST NOT share this URI with the sender.
- `senderURI` (string): sender URI `SU` of the connection that MUST be used as the endpoint for requests to send the messages.


#### Secure connection

PUT: recipient connection URI `<RU>`

Example: PUT `https://example.com/connection/aZ9f`

To secure the connection, simplex messaging client MUST send PUT request to the recipient connection URI `RU` (returned by the server when creating the connection). The request MUST be signed with the key `RK`.

Request body should be sent as JSON object with the following properties:
- `sender` (string): public key `SK` to verify digital signature of the sender.

If the connection was previously secured and the sender key is already set, the server MUST reject the request with HTTP status code 401 (Unauthorised).

If the connection is successfully secured, the server MUST respond with HTTP status code 200 (OK) without body.

Once the sender key is set, all the following unsigned requests from the sender (or signed with the wrong key) MUST be rejected with HTTP status code 401 (Unauthorised).


#### Delete connection

DELETE: recipient connection URI `<RU>`

Example: DELETE `https://example.com/connection/aZ9f`

To delete the connection, simplex messaging client MUST send DELETE request to the recipient connection URI `RU` (returned by the server when creating the connection), signed with the key `RK`.

If the connection deletion succeeded, the server MUST respond with HTTP status code 200 (OK) without body.

Server MUST permanently delete the connection and all unretrieved messages without preserving any copy of the connection or messages.

All further requests to the recipient and sender connection URIs MUST be rejected with HTTP status code 404 (Not Found).


#### Retrieve messages

GET: `<RU>/messages[?fromMessageId=<messageId>]`

Example: GET `https://example.com/connection/aZ9f/messages`

To retrieve messages from the connection, simplex messaging client MUST send POST request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` (it MUST NOT be changed by any implementation or deployment). The request MUST be signed with the key `RK`.

Optional query string parameter can be used for pagination:
- `fromMessageId` (string, optional): if set, the server will retrieve the messages received starting from the message with server message ID (unique per server) passed in this parameter.

Simplex messaging server will NOT delete the messages when this endpoint is called, to remove messages once retrieved [Delete message](#delete-message) MUST be called.

__Please note__: server implementations MUST NOT track in any form how many times or whether the messages were retrieved.

If the unknown message ID is passed in `fromMessageId` parameter, the request should be rejected with HTTP status code 404 (Not Found).

If the request is successful, the server MUST respond with HTTP status code (200) returning as response body not more than `PAGE_SIZE` (as configured in the server) of the earliest sent messages with the following properties:
- `messages` (array): retrieved messages. Each retrieved message is an object with the following properties:
   - `id` (string): server-generated unique ID allowing to identify messages until (they are deleted from the server) and to paginate responses.
   - `ts`(string) : server timestamp of the time when the message was received from the sender.
   - `size` (number): message size, in bytes.
   - `msg` (string, optional): encrypted message body, that the recipient should be able to decrypt with the key `EK`. This field is not returned if the message is larger than `LARGE_MESSAGE` (configured in the server), large messages can be retrieved via [Retrieve message](#retrieve_message) endpoint.
- `nextMessageID` (string, optional): if server has more messages available it MUST return this parameter with the ID of the next available message - it can be used by the next request in `fromMessageId` query string parameter.


#### Retrieve message

GET: `<RU>/messages/<messageId>`

Example: GET `https://example.com/connection/aZ9f/messages/1234`

To retrieve single message from the connection (e.g. large message), simplex messaging client MUST send POST request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages/<messageId>` (it MUST NOT be changed by any implementation or deployment). The request MUST be signed with the key `RK`.

Simplex messaging server will NOT delete the message when this endpoint is called, to remove the message once retrieved [Delete message](#delete-message) MUST be called.

__Please note__: server implementations MUST NOT track in any form how many times or whether the messages were retrieved.

If the unknown message ID is passed in `messageId` parameter, the request should be rejected with HTTP status code 404 (Not Found).

If the request is successful, the server MUST respond with HTTP status code (200) returning as response body the requested message with the following properties:
- `id` (string): server-generated unique ID allowing to identify messages until (they are deleted from the server) and to paginate responses.
- `ts` (string): server timestamp of the time when the message was received from the sender.
- `size` (number): message size, in bytes.
- `msg` (string): encrypted message body, that the recipient should be able to decrypt with the key `EK`.


#### Delete message

DELETE: `<RU>/messages/<messageId>`

Example: DELETE `https://example.com/connection/aZ9f/messages/1234`

To delete a single message from the connection, simplex messaging client MUST send DELETE request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` and message ID. The request MUST be signed with the key `RK`.

Simplex messaging clients MUST use this endpoint to delete the previously retrived and stored (or processed) large messages.

Simplex messaging server MUST permanently remove the message.

If the request is successful, the server MUST respond with HTTP status code (200).

If the unknown message ID is passed in request URI, the request should be rejected with HTTP status code 404 (Not Found).


### REST API endpoints for the connection sender

#### Send message

POST: `<SU>/messages`

Example: POST `https://example.com/connection/bY1h/messages`

To send message to the connection, simplex messaging client MUST send POST request to the recipient connection URI `RU` (returned by the server when creating the connection) with the REQUIRED appended string `/messages` (it MUST NOT be changed by any implementation or deployment), signed with the key `SK`.

Request body MUST be sent as JSON object with the following properties:
- `msg` (string): encrypted message body, that the recipient should be able to decrypt with the key `EK`. Any message meta-data (client timestamp, ID, etc.) MUST be inside the encrypted message and MUST NOT passed via additional properties.

If the request is successful, the server MUST respond with HTTP status code 200 (OK) without body.


## WebSockets API

TODO

**Simplex connection operation:**

![Simplex connection operations](/diagrams/simplex-messaging-impl/simplex-op.svg)

Sequence diagram does not show E2EE - connection itself knows nothing about encryption between sender and receiver.


[1]: simplex-messaging.md
[2]: https://tools.ietf.org/html/rfc3447
[3]: graph-chat.md
[4]: https://en.wikipedia.org/wiki/Forward_secrecy
