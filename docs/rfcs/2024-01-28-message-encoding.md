# Space-efficient chat message encoding

## Problem

We use JSON format to encode messages in SimpleX Chat protocol. It has many advantages:
- human readable, unlike binary formats.
- easy to extend.
- relatively low overhead for large text messages (and the overhead is not important for small messages, as we send fixed size blocks).
- ability to use Internet RFC 8927 for message format schema.

The main overhead of this format is for images that are included as text base64 encoded data in JSON for image and video previews, link previews (limited to 14,000 bytes of base64 encoded size) and profile pictures (limited to 12,500 of base64 encoded size, to account for the connection links sent alongside the profile images).

Adding post quantum encryption requires adding additional message headers with additional keys requiring up to 2500 additional bytes of cryptographic data in each message header. This is a significant overhead for messages with images and would not be compatible with the current constraints.

## Possible solutions

- reduce image preview sizes. The downside is that it would further reduce the quality of the image previews and profile pictures, that is already suboptimal.
- switch to a binary format for messages. The downside is that, depending on the format, it may be more difficult to extend and debug, and in any case it is not human readable.
- allow larger client messages than the block size, by extending SMP agent protocol. This is possible, but 1) quite complex, 2) leaks information when an image preview is sent, as multiple messages would be required sent in rapid succession (or some additional throttling logic).
- use hybrid format combining JSON format for the text and metadata part of the message, and binary format for the image previews and profile pictures.
- use some other binary format, e.g. MessagePack or CBOR.
- compress messages, e.g. using zstd.

The latter option looks attractive, as it avoids the complexities and downsides of the other options, while allows to provide the space for additional cryptographic data in the message headers without reducing image quality.

## Hybrid format

Currently we support two formats in chat protocol messages:

- JSON, which is identified by the first byte of the message being `{` or `[` (for batch of multiple messages sent in a single block, e.g. when message history or group introductions are sent).
- deprecated binary format for small chunks of files sent via SMP protocol that is only used for small voice messages, and only in case the user disables local file encryption. This format is identified by the first byte of the message being `F` (for "file").

The proposed format would use this format, using ABNF notation:

```abnf
hybridMessage = jsonPart "," binaryPart
jsonPart = "J" partLen ":" json
binaryPart = "B" partLen ":" 1*OCTET
partLen = 1*DIGIT
```

We could use a standart multipart format, but it seems unnecessarily generic and complex, and also more wasteful.

This syntax is sufficiently generic and extensible, and can be used for messages with more than two parts if necessary.

The downside is that it is ad-hoc, and does not achieve any possible reduction for other binary fields in JSON.

## MessagePack or CBOR

Using [MessagePack](https://github.com/msgpack/msgpack/blob/master/spec.md) or CBOR instead of JSON is another possible option, as it's both compact and efficient. While it's not human readable, it will result not only in more efficient binary encoding for images, but for all fields (such as hashes and member IDs, for example), and will also result in more efficient batching of small messages.

As we need to maintain backwards compatibility, we need to recognise MessagePack format, and as it has no distinct first byte of its own, we could use some fixed letter for it, e.g. `X` (we use `x` as a namespace prefix for all protocol message types).

The downside is implementation complexity, particularly given that historically use different binary encoding in JSON (base64 for images and base64url for other binary fields), so we would have to do one of the following:
- maintain two different encodings for all types that are sent between the clients, some of these encodings are manual.
- implement alternative AST for extended JSON format with binary support.
- in either case, types representing binary data would have to support decoding from both JSON strings and from binary data.

## Message compression

This might be the simplest option, as it does not require any changes to protocol encoding other than adding compression and have a different encoding for batching that can be the same as for SMP protocol (we could reuse the same function). The syntax for chat packet would be:

```abnf
chatPacket = %s'X' count 1*(message)
count = OCTET ; up to 255 messages in the batch
message = length compressedMessage
length = 2*2 OCTET ; length of compressed message, up to 65535 bytes (we have less than 15000 bytes limit).
```

After the character 'X' the encoding is the same as for SMP batches, so the same function can be used.

## Additional considerations

We do not just send images as base64 binary, we use web format for images, with the different prefixes for jpg and png images. In the client UIs though we simply discard these prefixes, and they are not processed. So we could use pure binary format for image data without any prefix.

We also use base64 format with prefix to send images in the API. We could continue using it, or we could remove prefix from the API and use plain base64.

base64 format with prefix also saved to the database, so we would need to parse and strip the prefix. Alternatively, we could do some data migration (it is likely to be slow) or export images to storage, at least for profile images - that would reduce overhead. This is not directly related to the problem we are solving here, and can be considered separately.
