# Chat protocol

## Design constraints

- the transport message has a fixed size (8 or 16kb)
- the chat message can have multiple parts/attachments
- the chat message including attachments can be of any size
- if the message is partially received, it should be possible to parse and display the received parts

## Questions

- should content types be:
  - limited to MIME-types
  - separate content types vocabulary
  - both MIME types and extensions
  - allow additional content types namespaces

## Message syntax

The syntax of the message inside agent MSG:

```abnf
agentMessageBody = message / msgContinuation
message = [chatMsgId] SP msgEvent SP [parameters] SP [contentParts [SP msgBodyParts]]
chatMsgId = 1*DIGIT ; used to refer to previous message;
                    ; in the group should only be used in messages sent to all members,
                    ; which is the main reason not to use external agent ID -
                    ; some messages are sent only to one member
msgEvent = protocolNamespace 1*("." msgTypeName)
protocolNamespace = 1*ALPHA ; "x" for all events defined in the protocol
msgTypeName = 1*ALPHA
parameters = parameter *("," parameter)
parameter = 1*(%x21-2B / %x2D-7E) ; exclude control characters, space, comma (%x2C)
contentParts = contentPart *("," contentPart)
contentPart = contentTypeNamespace "." contentType ":" contentSize [":" contentHash]
contentType = "i." <mime-type> / contentTypeNamespace "." 1*("." contentTypeName)
contentTypeNamespace = 1*ALPHA
contentTypeName = 1*ALPHA
contentHash = <base64>
msgBodyParts = msgBodyPart *(SP msgBodyPart)
msgEventParents = msgEventParent *msgEventParent ; binary body part for content type "x.dag"
msgEventParent = memberId refMsgId refMsgHash
memberId = 8*8(OCTET) ; shared member ID
refMsgId = 8*8(OCTET) ; sequential message number - external agent message ID
refMsgHash = 16*16(OCTET) ; SHA256 of agent message body
msgContinuation = "#" prevMsgId "." continuationId continuationData
```

### Example: messages, updates, groups

```
"3 x.msg.new c.text c.text:5 hello "
"4 x.msg.new c.image i.image/jpg:256,i.image/png:4096 abcd abcd "
"4 x.msg.new c.image x.dag:32,i.image/jpg:8000:hash1,i.image/png:16000:hash2 binary1"
"#4.1 binary1end binary2"
"#4.2 binary2continued"
"#4.3 binary2end "
"5 x.msg.new c.image i.image/jpg:256,i.image/url:160 abcd https://media.example.com/asdf#abcd "
'6 x.msg.update 3 c.text:11,x.dag:16 hello there abcd '
'7 x.msg.delete 3'
'8 x.msg.new app/v1 i.text/html:NNN,i.text/css:NNN,c.js:NNN,c.json:NNN ... ... ... {...} '
'9 x.msg.eval 8 c.json:NNN {...} '
'10 x.msg.new c.text 2 c.text:16,x.dag:32 hello there @123 abcd '
' x.grp.mem.inv 23456,123 1 c.json NNN {...} '
' x.grp.mem.acpt 23456 1 c.text NNN <invitation> '
' x.grp.mem.intro 23456,234 1 c.json NNN {...} '
' x.grp.mem.inv 23456,234 1 c.text NNN <invitation> '
' x.grp.mem.req 23456,123 1 c.json NNN {...} '
' x.grp.mem.direct.inv 23456,234 1 text NNN <invitation>  '
```
