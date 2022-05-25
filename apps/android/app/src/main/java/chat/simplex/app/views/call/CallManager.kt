package chat.simplex.app.views.call

import android.util.Log
import chat.simplex.app.TAG
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.helpers.withApi

class CallManager(val chatModel: ChatModel) {
  fun reportNewIncomingCall(invitation: CallInvitation) {
    Log.d(TAG, "CallManager.reportNewIncomingCall")
    with (chatModel) {
      callInvitations[invitation.contact.id] = invitation
      activeCallInvitation.value = invitation
      controller.ntfManager.notifyCallInvitation(invitation)
    }
  }

  fun answerIncomingCall(invitation: CallInvitation) {
    with (chatModel) {
      callInvitations.remove(invitation.contact.id)
      activeCall.value = Call(
        contact = invitation.contact,
        callState = CallState.InvitationAccepted,
        localMedia = invitation.peerMedia,
        sharedKey = invitation.sharedKey
      )
      showCallView.value = true
      callCommand.value = WCallCommand.Start (media = invitation.peerMedia, aesKey = invitation.sharedKey)
      if (invitation.contact.id == activeCallInvitation.value?.contact?.id) {
        activeCallInvitation.value = null
        controller.ntfManager.cancelCallNotification()
      }
    }
  }

  suspend fun endCall(call: Call) {
    with (chatModel) {
      if (call.callState == CallState.Ended) {
        Log.d(TAG, "CallManager.endCall: call ended")
        activeCall.value = null
        showCallView.value = false
      } else {
        Log.d(TAG, "CallManager.endCall: ending call...")
        callCommand.value = WCallCommand.End
        showCallView.value = false
        controller.apiEndCall(call.contact)
        activeCall.value = null
      }
    }
  }

  fun endCall(invitation: CallInvitation) {
    with (chatModel) {
      callInvitations.remove(invitation.contact.id)
      if (invitation.contact.id == activeCallInvitation.value?.contact?.id) {
        activeCallInvitation.value = null
        controller.ntfManager.cancelCallNotification()
      }
      withApi {
        if (!controller.apiRejectCall(invitation.contact)) {
          Log.e(TAG, "apiRejectCall error")
        }
      }
    }
  }

  fun reportCallRemoteEnded(invitation: CallInvitation) {
    if (chatModel.activeCallInvitation.value?.contact?.id == invitation.contact.id) {
      chatModel.activeCallInvitation.value = null
      chatModel.controller.ntfManager.cancelCallNotification()
    }
  }
}