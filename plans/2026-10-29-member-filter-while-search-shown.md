# Group and channel members stay filtered after the search field disappears

## Problem

With a search typed in a group of 9 members besides you, when a member leaves, the search field is hidden, and members that do not match stay hidden until the info is closed. The same applies to the channel members page ("Subscribers" for moderators, "Owners & contributors" for others) when a listed member is removed or leaves.

## Cause

The search field is shown only above 8 members (`GroupChatInfoLayout`, `ChannelMembersView`), while the filter applies whenever the text is not empty. The text is stored in the page's `ModalData` (#4817 group info, #7264 channel members), so it remains after the field leaves composition.

## Fix

The filter applies only while the field is shown: `!showMemberSearch || s.isEmpty()` in group info, `!showSearch || s.isEmpty()` for both lists of `ChannelMembersView`. The stored text filters the list again once the field is shown.

## Scope

Android and desktop.
