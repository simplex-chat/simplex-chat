package chat.simplex.common.views.call

import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.datetime.Clock
import kotlin.time.Duration.Companion.minutes

class CallManager(val chatModel: ChatModel) {
  fun reportNewIncomingCall(invitation: RcvCallInvitation) {
    Log.d(TAG, "CallManager.reportNewIncomingCall")
    with (chatModel) {
      callInvitations[invitation.contact.id] = invitation
      if (invitation.user.showNotifications) {
        if (Clock.System.now() - invitation.callTs <= 3.minutes) {
          invitation.sentNotification = ntfManager.notifyCallInvitation(invitation)
          activeCallInvitation.value = invitation
        } else {
          val contact = invitation.contact
          ntfManager.displayNotification(user = invitation.user, chatId = contact.id, displayName = contact.displayName, msgText = invitation.callTypeText)
        }
      }
    }
  }

  fun acceptIncomingCall(invitation: RcvCallInvitation) = withBGApi {
    val call = chatModel.activeCall.value
    val contactInfo = chatModel.controller.apiContactInfo(invitation.remoteHostId, invitation.contact.contactId)
    val profile = contactInfo?.second ?: invitation.user.profile.toProfile()
    // In case the same contact calling while previous call didn't end yet (abnormal ending of call from the other side)
    if (call == null || (call.remoteHostId == invitation.remoteHostId && call.contact.id == invitation.contact.id)) {
      justAcceptIncomingCall(invitation = invitation, profile)
    } else {
      chatModel.switchingCall.value = true
      try {
        endCall(call = call)
        justAcceptIncomingCall(invitation = invitation, profile)
      } finally {
        chatModel.switchingCall.value = false
      }
    }
  }

  private fun justAcceptIncomingCall(invitation: RcvCallInvitation, userProfile: Profile) {
    with (chatModel) {
      activeCall.value = Call(
        remoteHostId = invitation.remoteHostId,
        userProfile = userProfile,
        contact = invitation.contact,
        callState = CallState.InvitationAccepted,
        localMedia = invitation.callType.media,
        sharedKey = invitation.sharedKey,
      )
      showCallView.value = true
      val useRelay = controller.appPrefs.webrtcPolicyRelay.get()
      val iceServers = getIceServers()
      Log.d(TAG, "answerIncomingCall iceServers: $iceServers")
      callCommand.add(WCallCommand.Start(
        media = invitation.callType.media,
        aesKey = invitation.sharedKey,
        iceServers = iceServers,
        relay = useRelay
      ))
      callInvitations.remove(invitation.contact.id)
      if (invitation.contact.id == activeCallInvitation.value?.contact?.id) {
        activeCallInvitation.value = null
        ntfManager.cancelCallNotification()
      }
    }
  }

  suspend fun endCall(call: Call) {
    with(chatModel) {
      // If there is active call currently and it's with other contact, don't interrupt it
      if (activeCall.value != null && !(activeCall.value?.remoteHostId == call.remoteHostId && activeCall.value?.contact?.id == call.contact.id)) return

      // Don't destroy WebView if you plan to accept next call right after this one
      if (!switchingCall.value) {
        showCallView.value = false
        activeCall.value = null
        activeCallViewIsCollapsed.value = false
        platform.androidCallEnded()
      }
      if (call.callState == CallState.Ended) {
        Log.d(TAG, "CallManager.endCall: call ended")
      } else {
        Log.d(TAG, "CallManager.endCall: ending call...")
        //callCommand.add(WCallCommand.End)
        controller.apiEndCall(call.remoteHostId, call.contact)
      }
    }
  }

  fun endCall(invitation: RcvCallInvitation) {
    with (chatModel) {
      callInvitations.remove(invitation.contact.id)
      if (invitation.contact.id == activeCallInvitation.value?.contact?.id) {
        activeCallInvitation.value = null
        ntfManager.cancelCallNotification()
      }
      withBGApi {
        if (!controller.apiRejectCall(invitation.remoteHostId, invitation.contact)) {
          Log.e(TAG, "apiRejectCall error")
        }
      }
    }
  }

  fun reportCallRemoteEnded(invitation: RcvCallInvitation) {
    if (chatModel.activeCallInvitation.value?.contact?.id == invitation.contact.id) {
      chatModel.activeCallInvitation.value = null
      ntfManager.cancelCallNotification()
    }
  }
}
