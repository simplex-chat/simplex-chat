## Simplex messaging protocol REST API

This document lists all required REST endpoints of simplex messaging API.

Also see [Simplex messaging protocol implementation](simplex-messaging-implementation.md) for more details.

## POST /connection

### Create connection


### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"recipientKey":"BODbZxmtKUUF1l8pj4nVjQ"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"senderId":"N9pA3g","recipientId":"Qxz93A"}
```

## DELETE /connection/:connectionId

### Delete connection


### Captures:

- *connectionId*: Recipient connection ID - unique connection ID to be used by connection recipient

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## PUT /connection/:connectionId

### Secure connection


### Captures:

- *connectionId*: Recipient connection ID - unique connection ID to be used by connection recipient

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"senderKey":"XPaVEVNunkYKqqK0dnAT5Q"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## GET /connection/:connectionId/messages

### Get messages


### Captures:

- *connectionId*: Recipient connection ID - unique connection ID to be used by connection recipient

### GET Parameters:

- fromMessageId
     - **Values**: *message ID, e.g., `p8PCiGPZ`*
     - **Description**: if set, the server will respond with the messages received starting from the message with server message ID (unique per server) passed in this parameter.


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"messages":[{"ts":"2020-03-15T19:58:33.695Z","msg":"OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs","connId":"p8PCiGPZ"}],"nextMessageId":null}
```

## DELETE /connection/:connectionId/messages/:messageId

### Delete message


### Captures:

- *connectionId*: Recipient connection ID - unique connection ID to be used by connection recipient
- *messageId*: Message ID - unique message ID to be used by connection recipient

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## POST /connection/:senderConnectionId/messages

### Send message


### Captures:

- *senderConnectionId*: Sender connection ID - unique connection ID to be used by connection sender

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"msg":"OQLMXoEA4iv-aR46puPJuY1Rdoc1KY0gfq8oElJwtAs"}
```

### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

