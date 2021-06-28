# Chat protocol

## Message syntax

The syntax of the message inside agent MSG:

```abnf
agentMessageBody = message / msgContinuation
message = [chatMsgId] SP msgEvent SP [parameters] SP msgBody SP
chatMsgId = 1*DIGIT ; used to refer to previous message;
                    ; in the group should only be used in messages sent to all members,
                    ; which is the main reason not to use external agent ID -
                    ; some messages are sent only to one member
msgEvent = protocolNamespace 1*("." msgTypeName)
protocolNamespace = 1*lowercase_letter ; "x" for all messages defined in the protocol
msgTypeName = 1*lowercase_letter
lowercase_letter = %x61-7A ; a-z
parameters = parameter *("," parameter)
parameter = 1*(%x21-2B / %x2D-7E) ; exclude control characters, space, comma (%x2C)
msgBody = partNum SP msgBodyPart *(SP msgBodyPart) ; the number of parts should match the number of contentTypes
partNum = 1*DIGIT ; number of message body parts
msgBodyPart = contentType SP encoding [totalPartSize ":"] partSize SP msgData SP
contentType = contentMainType ["/" contentSubType ["+" contentSubSubType]] ; should it be MIME media type?
contentMainType = 1*ALPHA ; e.g. "text", "image", "app", etc.
contentSubType = 1*ALPHA ; e.g. "png", "v1", etc.
contentSubSubType = 1*ALPHA
encoding = %s"b" / %s"t" / %s"j" ; binary, text or JSON ; is it needed at all?
                                                        ; or should it be determined by contentType?
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
msgContinuation = "#" prevMsgId SP partSize SP msgData *(SP msgBodyPart)
```

### Example: text message and updates

```
"3 x.msg.new text 1 text t5 hello  "
"4 x.msg.new image 2 image/jpg b256 abcd  image/png b4096 abcd  "
"5 x.msg.new image 3 image/jpg b256 abcd  image/url t160 https://media.example.com/asdf#abcd  "
'6 x.msg.update 3 1 text t11 hello there prev b16 abcd '
'7 x.msg.delete 3'
'8 x.msg.new app/v1 4 text/html tNNN ...  text/css tNNN ...  text/js tNNN ...  application/json jNNN {...}  '
'8 x.msg.eval 8 1 application/json jNNN {...}  '
' x.grp.mem.inv 23456,123 1 application/json jNNN {...}  '
' x.grp.mem.acpt 23456 1 text tNNN invitation '
' x.grp.mem.intro 23456,234 1 application/json jNNN {...}  '
' x.grp.mem.inv 23456,234 1 text tNNN invitation  '
' x.grp.mem.req 23456,123 1 application/json jNNN {...}  '
' x.grp.mem.direct.inv 23456,234 1 text tNNN invitation  '
```