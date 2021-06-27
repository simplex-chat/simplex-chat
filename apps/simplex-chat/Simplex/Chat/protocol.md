# Chat protocol

## Message syntax

The syntax of the message inside agent MSG:

```abnf
agentMessageBody = message / msgContinuation
message = msgEvent SP [parameters] SP msgBody
msgEvent = protocolNamespace 1*("." msgTypeName)
protocolNamespace = 1*lowercase_letter ; "x" for all messages defined in the protocol
msgTypeName = 1*lowercase_letter
lowercase_letter = %x61-7A ; a-z
parameters = parameter 1*("," parameter)
parameter = <1+ characters without control characters, space, comma>
msgBody = msgBodyPart *(SP msgBodyPart) ; the number of parts should match the number of contentTypes
msgBodyPart = contentType SP encoding [totalPartSize ":"] partSize SP msgData SP
contentType = contentMainType ["/" contentSubType ["+" contentSubSubType]] ; should it be MIME media type?
contentMainType = 1*ALPHA ; e.g. "text", "image", "app", etc.
contentSubType = 1*ALPHA ; e.g. "png", "v1", etc.
contentSubSubType = 1*ALPHA
encoding = %s"b" / %s"t" / %s"j" ; binary, text or JSON
msgData = binMsgData / txtMsgData / jsonMsgData
binMsgData = *OCTET ; binary body
txtMsgData = <utf-8 encoded text>
jsonMsgData = <JSON as utf-8 encoded text>
partSize = 1*DIGIT ; size of this part within this message
totalPartSize = 1*DIGIT ; total size of this part across all messages, including the current one
msgEventParents = msgEventParent *msgEventParent ; binary body part for content type "prev"
msgEventParent = memberId refMsgId refMsgHash
memberId = 8*8(OCTET) ; shared member ID
refMsgId = 8*8(OCTET) ; sequential message number - external agent message ID
refMsgHash = 16*16(OCTET) ; SHA256 of agent message body
msgContinuation = "#" prevMsgId SP encoding partSize SP msgData *(SP msgBodyPart)
```

### Example: text message and updates

```
"x.msg.new text text t5 hello "
"x.msg.new image image/jpg b256 abcd image/png b4096 abcd "
"x.msg.new image image/jpg b256 abcd image/url t160 https://media.example.com/asdf#abcd "
'x.msg.update 3 text t11 hello there prev b16 abcd '
'x.msg.delete 3'
'x.msg.new app/v1 text/html tNNN ... text/css tNNN ... text/js tNNN ... application/json jNNN {...} '
'x.msg.eval 4 application/json jNNN {...} '
'x.grp.mem.inv 23456 application/json jNNN {...} '
'x.grp.mem.new 23456 application/json jNNN {...} '
```