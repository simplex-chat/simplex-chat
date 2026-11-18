# UX design principles

General principles for any user interface, drawn from the names store's design iteration of 21–23 October 2026.

1. **Stable layout.** The main controls keep their position across steps and states. Content that can change (a status, an error, a late result) has space reserved for it; a change never pushes or shifts anything else.
2. **One main action per screen.** It is the most prominent control, in a predictable place; other actions are visibly secondary. A required condition, such as a consent, sits just before the action, which stays disabled until it is met.
3. **Design for the extremes.** Every screen is designed and checked with the longest and the shortest real content, empty and full states, every supported width, light and dark, and slow and failed responses.
4. **Exact spacing.** Gaps come from a fixed scale and stay the same for equivalent elements whatever their content. Inline elements sit on the text's baseline; icons are centred on their text.
5. **Say what is true now.** State facts with when they were checked, keep them current while they are shown, and update them in place as soon as they change. Promise nothing the system does not guarantee.
6. **Words match what things are.** Name each thing by what it is to the user, and never describe one concept in the words of another: something free is not a payment, a balance is not a fee. A term means the same on every screen.
7. **Offer only what can work.** An option that cannot succeed in the current state is not offered; a disabled control shows why. Prevent expected failures rather than report them.
8. **What looks interactive is interactive.** A control responds over its whole visible area, and nothing that is not a control looks like one.
9. **Fewest words, said once.** Each fact appears once, where it is needed. Short sentences; no repeats between neighbouring elements.
10. **No dead ends.** Every state, including errors and rare ones, says what happened and offers the next step. When only a person can help, give the contact and the reference they will need.
11. **Money and irreversible actions are safe.** They need an explicit act, survive interruption and reload, and never happen twice.
12. **Motion explains.** Transitions connect one state to the next; nothing flashes, flickers or visibly loads in.
13. **Verify by measurement.** Check layouts by measuring positions and gaps in real browsers at every width and theme, not by eye or screenshots alone.
