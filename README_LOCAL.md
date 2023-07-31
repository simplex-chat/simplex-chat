EditingOperation can only be “added” or “removed”, right?

So in the UI what is added will be green, and what is removed - red, so that’s where the type was coming from – how it will show in UI.
Talking about Bool vs Enum - I agree that when we consider that extensibility may be needed at some future point, and it is database or protocol encoding rather than just internal API, then Enum is indeed better. In this case we are 100% certain that there will be no other editing operations, and also it’s easy to change internal API if needed, so `added :: Maybe Bool` seems both more semantically correct than `operation :: EditingOperation` and also simpler - we don’t need to define the same Enum in three different languages and also define its JSON encodings - Bool just works out of the box.

Not that important whether we use Text or String, but String seemed easier to process if we write this code ourselves, but if we use some external dependency for it and it only supports Text for some reason, than it doesn’t matter - they both encode to JSON the same. Performance difference between Text and String in client environment is negligible in most cases.
====================

refs

https://www.reddit.com/r/haskell/comments/13q1q4k/implementation_for_discovering_string_edits/

https://hackage.haskell.org/package/Diff-0.4.1/docs/Data-Algorithm-Diff.html

https://en.wikipedia.org/wiki/Longest_common_substring

---------------
https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/

https://blog.jcoglan.com/2017/02/15/the-myers-diff-algorithm-part-2/

https://blog.jcoglan.com/2017/02/17/the-myers-diff-algorithm-part-3/

https://blog.jcoglan.com/2017/03/22/myers-diff-in-linear-space-theory/

https://blog.jcoglan.com/2017/04/25/myers-diff-in-linear-space-implementation/

---------------
https://hackage.haskell.org/package/myers-diff