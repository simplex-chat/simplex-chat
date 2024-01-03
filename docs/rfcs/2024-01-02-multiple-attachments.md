# Sending multiple files with a message

## Problem

The immediate problem we need to solve is encrypting local videos, the absense of which creates a lot of confusion and perception that the videos are sent unencrypted.

The reason videos are not encrypted is because they are usually large files that would be slow to decrypt in order to play, and they are also used to generate previews on the fly. The latter is slow anyway, and causes bad rendering experience.

Videos on iOS are compressed from 5.4.2, and the solution to keep them encrypted is to keep video preview in a separate file. It can be done either on the receiving side, when the video is first rendered, or on the sending side, in which case we can send the preview as a separate file.

In general, attaching multiple files to a message could be beneficial for other cases, such as sending hi- and low-res images at the same time, or sending long-form messages with attachments.

## Solutions

1. Extend chat protocol to allow multiple attachments in a single protocol message. This PR has types to support it, and database schema already supports multiple files with chat item. Different message types can allow a limited number of files and interpret them according to their indices in the array. This seems an ad hoc approach, as semantics of the additional attachments are not defined in the protocol and should be implied by file positions.

2. Still allow multiple attachments but add file semantics in the protocol message, alongside FileInvitation object, e.g.:

```
data FileAttachment = FileAttachment
  { file :: FileInvitation,
    fileType :: FileAttachmentType
  }
```

This format is marginally more complex, but it is more extensible.

3. Instead of allowing multiple attachments in a message, we could allow up to two files for a single attachment (which is what we need now), that could later be useful for messages with multiple attachments as well. This way FileInvitation will be replaced with:

```
data FileAttachment = FileAttachment
  { preview :: Maybe FileInvitation, -- received automatically if "receive images" is enabled
    file :: FileInvitation, -- received automatically if "receive images" is enabled for images, in the absense of preview
  }
```

4. Add additional protocol message to send additional attachments separately.

5. To solve only original problem of videos, we could add an API to save previews on the first render - this seems the worst approach, as it both complicates the logic of the recipient, without allowing other use cases.
