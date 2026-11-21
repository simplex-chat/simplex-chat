# Wireframe collaboration without a shared agent

Two developers work the same UX flow. Each has an LLM agent, and the agents share nothing:
no session, no memory, no canvas. The thing under review is a picture. The fix is to make
the diagram a source file. It lives in the repo, both humans edit it in their IDE, both
agents read and write it directly, and every change is inspected as a rendered image.

## The loop

**1. Generate.** Alice prompts her agent to write the flow as an `.excalidraw` file, asking
for the wireframe and for a file that stays comfortable to edit by hand. The second half is
what agents get wrong by default, so it is spelled out below.

**2. Edit.** Alice opens it in her IDE with the
[Excalidraw Editor plugin](https://plugins.jetbrains.com/plugin/32029-excalidraw-editor)
(id 32029, by Agustin). Same window as the code, same worktree, no export step.

**3. Diff.** She renders both revisions with
[excalirender](https://github.com/JonRC/excalirender) and flips between them as an animated
GIF at the same pixel alignment, so anything that moved flickers. This is also how she
checks what her agent did when she asked for an edit.

**4. Review.** She commits to `plans/sketches/` and asks Bob, who edits the diagram in his
own IDE and commits back, or reviews in text on GitHub. His agent reads the same file and
needs nothing from Alice's.

**5. Iterate.** Alice renders Bob's commit against its parent and goes back to step 1 or 2.

The repo is the channel, the file is the message, the render is the review surface.

example PR: https://github.com/simplex-chat/simplex-chat/pull/7525

## Conventions

For the agent. Follow these when generating or editing an `.excalidraw` file here.

### Keep it editable by hand

- **Group every panel.** Each screen, alert or card is one group, so dragging moves the box,
  its labels and its buttons together. Nest a second level for a row or a band.
- **Bind both ends of every arrow**, through `startBinding` and `endBinding` plus the
  arrow's id in each target's `boundElements`. An unbound arrow detaches on the first drag.
  Lines are not bindable, so where arrows fan out from a rail, make that rail a very thin
  rectangle rather than a line.
- **Elbow arrows** store the attachment as `fixedPoint` on the binding. Dropping an endpoint
  onto a target a fraction of a pixel wide resets it to the side midpoint, so set the value
  instead of dragging.
- **Labels in containers.** A text with a `containerId` moves and wraps with its box.
- **One multiline text per paragraph.** Lines centred inside a card stay separate elements.
- One flow per file, named `YYYY-MM-DD-topic.excalidraw`, in `plans/sketches/`.

### Draw primitives, not cosmetics

Boxes, arrows and text in black, plus one accent colour for whatever is tappable. No fills,
rounded corners, phone bezels, avatars or status bars. Group sections with a filled,
borderless rectangle behind them in a desaturated tint, one per section. Rows share a single
y and a fixed column pitch, and a caption sits 7.4px under its card, left edges aligned.

### Use fontFamily 9

Liberation Sans. Never 2, Helvetica: excalirender does not bundle it, falls back to
monospace, and renders every string at 140% of its stored width, so every card overflows.
Both tools bundle 9, and it is metric-compatible with Helvetica.

### Normalize before every commit

The plugin writes the full element schema, pretty-printed, on every save: 14k lines and
~90k tokens for a 354-element sketch. `plans/sketches/normalize-excalidraw.py` drops the
fields Excalidraw re-derives on load and writes one element per line, and
`.git/hooks/pre-commit` runs it over staged files. A changed element then costs one changed
line: a real commit went from 219 insertions and 826 deletions to 28 and 43.

Never add a field to its strip list without a plugin round trip to prove it. `strokeWidth`
defaults to 2, so dropping a 1 doubles every stroke in the drawing, and `lineHeight` is
recomputed from font metrics when absent.

### Diff visually, not textually

```
git show <parent>:<path> > /tmp/old.excalidraw
excalirender /tmp/old.excalidraw -o old.png -s 2
excalirender <path> -o new.png -s 2
excalirender diff /tmp/old.excalidraw <path>    # element counts and tags
```

`excalirender diff` classifies correctly but renders only the changed elements, which on a
sketch where 42 of 354 changed is a near-blank canvas. Build the flip from two full renders
and align them by content bounding box: the tool crops with a 20px margin, so a revision
whose box shifted renders at a different offset and the flip jitters.

Arrow labels are drawn where they are stored rather than recomputed, so keep them about
16px clear of the line or excalirender strikes through them.

### Report in the reviewer's terms

Name panels by the labels a human reads on them. "Panel 4e removed, the price in 2c went
from $20 to $100" is reviewable. "15 elements removed, 27 modified" is only the headline.
