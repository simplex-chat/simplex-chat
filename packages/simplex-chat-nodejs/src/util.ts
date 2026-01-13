import {T} from "@simplex-chat/types"
import {BotAddressSettings} from "./api"

export function chatInfoRef(cInfo: T.ChatInfo): T.ChatRef | undefined {
  switch (cInfo.type) {
    case T.ChatType.Direct: return {chatType: T.ChatType.Direct, chatId: cInfo.contact.contactId}
    case T.ChatType.Group: {
      const chatScope: T.GroupChatScope | undefined =
        cInfo.groupChatScope?.type == "memberSupport"
        ? {type: "memberSupport", groupMemberId_: cInfo.groupChatScope.groupMember_?.groupMemberId}
        : undefined
      return {chatType: T.ChatType.Group, chatId: cInfo.groupInfo.groupId, chatScope}
    }
    default: return undefined
  }
}

export function chatInfoName(cInfo: T.ChatInfo): string {
  switch (cInfo.type) {
    case "direct": return `@${cInfo.contact.profile.displayName}`
    case "group": {
      const scope = cInfo.groupChatScope
      const scopeName = scope?.type === "memberSupport"
                        ? `(support${scope.groupMember_ ? ` ${scope.groupMember_.memberProfile.displayName}` : ""})`
                        : ""
      return `#${cInfo.groupInfo.groupProfile.displayName}${scopeName}`
    }
    case "local": return "private notes"
    case "contactRequest": return `request from @${cInfo.contactRequest.profile.displayName}`
    case "contactConnection": {
      const alias = cInfo.contactConnection.localAlias
      return `pending connection${alias ? ` (@${alias})` : ""}`
    }
  }
}

export function senderName(cInfo: T.ChatInfo, chatDir: T.CIDirection) {
  const sender = chatDir.type === "groupRcv"
                  ? ` @${chatDir.groupMember.memberProfile.displayName}`
                  : ""
  return chatInfoName(cInfo) + sender
}

export function contactAddressStr(link: T.CreatedConnLink): string {
  return link.connShortLink || link.connFullLink
}

export function botAddressSettings({addressSettings}: T.UserContactLink): BotAddressSettings {
  return {
    autoAccept: addressSettings.autoAccept ? true : false,
    welcomeMessage: addressSettings.autoReply,
    businessAddress: addressSettings.businessAddress
  }
}

export function fromLocalProfile({displayName, fullName, shortDescr, image, contactLink, preferences, peerType}: T.LocalProfile): T.Profile {
  const profile = {displayName, fullName, shortDescr, image, contactLink, preferences, peerType}
  for (const key in profile) {
    if (typeof (profile as any)[key] === "undefined") delete (profile as any)[key]
  }
  return profile
}

export function ciContentText({content}: T.ChatItem): string | undefined {
  switch (content.type) {
    case "sndMsgContent": return content.msgContent.text;
    case "rcvMsgContent": return content.msgContent.text;
    default: return undefined;
  }
}

export interface BotCommand {
  keyword: string
  params: string
}

// returns command (without /) and trimmed parameters
export function ciBotCommand(chatItem: T.ChatItem): BotCommand | undefined {
  const msg = ciContentText(chatItem)?.trim()
  if (msg) {
    const r = msg.match(/\/([^\s]+)(.*)/)
    if (r && r.length >= 3) {
      return {keyword: r[1], params: r[2].trim()}
    }
  }
  return undefined
}

export function reactionText(reaction: T.ACIReaction): string {
  const r = reaction.chatReaction
  return r.reaction.type === "emoji" ? r.reaction.emoji : r.reaction.tag
}