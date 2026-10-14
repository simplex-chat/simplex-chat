package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs

object TestPrefs {
  fun preset() {
    with(appPrefs) {
      laNoticeShown.set(true)
      appUpdateNoticeShown.set(true)
      oneHandUICardShown.set(true)
      addressCreationCardShown.set(true)
      getStakeBannerDismissed.set(true)
      supporterBannerShown.set(true)
      showHiddenProfilesNotice.set(false)
      showMuteProfileAlert.set(false)
      showDeleteConversationNotice.set(false)
      showDeleteContactNotice.set(false)
      privacyDeliveryReceiptsSet.set(true)
      privacyLinkPreviewsShowAlert.set(false)
    }
  }
}
