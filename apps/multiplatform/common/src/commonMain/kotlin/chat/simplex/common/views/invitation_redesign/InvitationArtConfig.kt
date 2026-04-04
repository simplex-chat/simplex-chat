package chat.simplex.common.views.invitation_redesign

import chat.simplex.common.BuildConfigCommon

/**
 * True when `simplex-chat-art/multiplatform/light` was present at build time (see `common/build.gradle.kts`).
 * When false, resources come from placeholder SVGs — decorative invitation hero images must not be shown.
 */
internal val fullInvitationArtAvailable: Boolean
  get() = BuildConfigCommon.FULL_INVITATION_IMAGES
