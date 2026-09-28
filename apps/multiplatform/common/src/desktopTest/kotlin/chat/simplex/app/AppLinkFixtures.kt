package chat.simplex.app

internal const val BADGE_LINK_PREFIX = "simplexchat:/badge/code/"
internal const val BADGE_LINK = "${BADGE_LINK_PREFIX}SB0000000000000000000"
internal const val OTHER_BADGE_LINK = "${BADGE_LINK_PREFIX}SB1111111111111111111"

internal fun badgeLinkOfBytes(size: Int): String = BADGE_LINK_PREFIX + "A".repeat(size - BADGE_LINK_PREFIX.length)
