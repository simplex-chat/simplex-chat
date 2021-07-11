# Chat protocol

## Design constraints

- the transport message has a fixed size (8 or 16kb), but the SMP agent will be updated to support sending messages up to maximum configured size (TBC - 64-256kb) in 8-16Kb blocks.
- the chat message can have multiple content parts, but it should fit the agent message of the variable size.
- one of the chat message types should support transmitting large binaries in chunks that could potentially be interleaved with other messages. For example, image preview would fit the message, but the full size image will be transmitted in chunks later - same for large files.
- using object storage can be effective for large groups, but we will postpone it until content channels are implemented.

## Questions

- should content types be:
  - limited to MIME-types
  - separate content types vocabulary
  - both MIME types and extensions (currently we support MIME (m.) and Simplex (x.) namespaces)
  - allow additional content types namespaces

## Message syntax

The syntax of the message inside agent MSG:

```abnf
agentMessageBody = [chatMsgId] SP msgEvent SP [parameters] SP [contentParts [SP msgBodyParts]]
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
```

### Example: messages, updates, groups

```
"3 x.msg.new c.text x.text:5 hello "
"4 x.msg.new c.image i.image/jpg:256,i.image/png:4096 abcd abcd "
"4 x.msg.new c.image x.dag:32,i.image/jpg:8000,i.image/png:16000 binary1"
"5 x.msg.new c.image,i.image/jpg:150000 i.image/jpg:256 abcd "
"6 x.msg.file 5,1.1 x.file:60000 abcd "
"7 x.msg.file 5,1.2 x.file:60000 abcd "
"8 x.msg.file 5,1.3 x.file:30000 abcd "
'8 x.msg.update 3 x.text:11,x.dag:16 hello there abcd '
'9 x.msg.delete 3'
'10 x.msg.new app/v1 i.text/html:NNN,i.text/css:NNN,c.js:NNN,c.json:NNN ... ... ... {...} '
'11 x.msg.eval 8 c.json:NNN {...} '
'12 x.msg.new c.text x.text:16,x.dag:32 hello there @123 abcd '
' x.grp.mem.inv 23456,123 x.json:NNN {...} '
' x.grp.mem.acpt 23456 x.text:NNN <invitation> '
' x.grp.mem.intro 23456,234 x.json:NNN {...} '
' x.grp.mem.inv 23456,234 x.text:NNN <invitation> '
' x.grp.mem.req 23456,123 x.json:NNN {...} '
' x.grp.mem.direct.inv 23456,234 x.text:NNN <invitation>  '
```

### Group protocol

#### Add group member

A -> B: invite to group - `MSG: x.grp.inv G_MEM_ID_A,G_MEM_ROLE_A,G_MEM_ID_B,G_MEM_ROLE_B,<invitation> x.json:NNN <group_profile>`
user B confirms
B -> A: establish group connection (B: JOIN, A: LET)
B -> Ag: join group - `in SMP confirmation: x.grp.acpt G_MEM_ID_B`
A -> group (including B)): announce group member: `MSG: N x.grp.mem.new G_MEM_ID_B,G_MEM_ROLE_B x.json:NNN <B_profile>`
subsequent messages between A and B are via group connection
A -> Bg: intro member - `MSG: x.grp.mem.intro G_MEM_ID_M,G_MEM_ROLE_M x.json:NNN <M_profile>`
B -> Ag: inv for mem - `MSG: x.grp.mem.inv G_MEM_ID_M,<gr_invitation>,<dm_invitation>,<probe>`
M is an existing member, messages are via group connection
A -> Mg: fwd inv - `MSG: x.grp.mem.fwd G_MEM_ID_B,<gr_invitation>,<dm_invitation>,<probe>`
M -> Bg: establish group connection (M: JOIN, B: LET)
M -> B: establish direct connection (M: JOIN, B: LET)
M -> Bg: confirm profile and role - `MSG: x.grp.mem.info G_MEM_ID_M,G_MEM_ROLE x.json:NNN <M_profile>`
if M is a known contact (profile match) send probe to M:
  B -> M (via old DM conn): profile match probe: `MSG: x.grp.mem.probe G_MEM_ID_B,<probe_hash>`
  M -> B (via old DM conn): probe confirm: `MSG: x.grp.mem.probe.ok G_MEM_ID_M,<probe>`
  link to the same contact
B -> Ag: connected to M: `MSG: x.grp.mem.con G_MEM_ID_M`
M -> Ag: connected to M: `MSG: x.grp.mem.con G_MEM_ID_B`

once all members connected
A -> group: `MSG: N x.grp.mem.ok G_MEM_ID_B`

#### Send group message

`MSG: N x.msg.new G_MEM_ROLE,<invitation> x.json:NNN <group_profile>`
