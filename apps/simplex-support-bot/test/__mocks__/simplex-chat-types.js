// Mock for @simplex-chat/types — lightweight stubs

const ChatType = {Direct: "direct", Group: "group", Local: "local"}
const GroupMemberRole = {Member: "member", Owner: "owner", Admin: "admin", Relay: "relay", Observer: "observer", Author: "author", Moderator: "moderator"}
const GroupMemberStatus = {Connected: "connected", Complete: "complete", Announced: "announced", Left: "left", Removed: "removed", Invited: "invited"}
const GroupFeatureEnabled = {On: "on", Off: "off"}
const CIDeleteMode = {Broadcast: "broadcast", Internal: "internal"}
const SndCIStatusProgress = {Partial: "partial", Complete: "complete"}

module.exports = {
  T: {ChatType, GroupMemberRole, GroupMemberStatus, GroupFeatureEnabled, CIDeleteMode, SndCIStatusProgress},
  CEvt: {},
}
