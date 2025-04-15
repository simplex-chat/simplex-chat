package chat.simplex.common.views.chat.group

import InfoRow
import SectionBottomSpacer
import SectionDividerSpaced
import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.platform.chatModel
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

@Composable
fun MemberAdmissionView(m: ChatModel, rhId: Long?, chatId: String, close: () -> Unit) {
  val groupInfo = remember { derivedStateOf {
    val ch = m.getChat(chatId)
    val g = (ch?.chatInfo as? ChatInfo.Group)?.groupInfo
    if (g == null || ch.remoteHostId != rhId) null else g
  }}
  val gInfo = groupInfo.value ?: return
  var admission by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(gInfo.groupProfile.memberAdmission) }
  var currentAdmission by rememberSaveable(gInfo, stateSaver = serializableSaver()) { mutableStateOf(admission) }

  fun saveAdmission(afterSave: () -> Unit = {}) {
    withBGApi {
      val gp = gInfo.groupProfile.copy(memberAdmission = admission)
      val g = m.controller.apiUpdateGroup(rhId, gInfo.groupId, gp)
      if (g != null) {
        withContext(Dispatchers.Main) {
          chatModel.chatsContext.updateGroup(rhId, g)
          currentAdmission = admission
        }
      }
      afterSave()
    }
  }
  ModalView(
    close = {
      if (admission == currentAdmission) close()
      else showUnsavedChangesAlert({ saveAdmission(close) }, close)
    },
  ) {
    MemberAdmissionLayout(
      admission,
      currentAdmission,
      gInfo,
      applyAdmission = { admsn ->
        admission = admsn
      },
      reset = {
        admission = currentAdmission
      },
      saveAdmission = ::saveAdmission,
    )
  }
}

@Composable
private fun MemberAdmissionLayout(
  admission: GroupMemberAdmission?,
  currentAdmission: GroupMemberAdmission?,
  groupInfo: GroupInfo,
  applyAdmission: (GroupMemberAdmission) -> Unit,
  reset: () -> Unit,
  saveAdmission: () -> Unit,
) {
  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.member_admission))
    val review = remember(admission) { mutableStateOf(admission?.review) }
    AdmissionSection(MR.strings.admission_stage_review, MR.strings.admission_stage_review_descr, review, groupInfo) { criteria ->
      if (admission != null) {
        applyAdmission(admission.copy(review = criteria))
      } else {
        applyAdmission(GroupMemberAdmission(review = criteria))
      }
    }
    if (groupInfo.isOwner) {
      SectionDividerSpaced(maxTopPadding = true, maxBottomPadding = false)
      ResetSaveButtons(
        reset = reset,
        save = saveAdmission,
        disabled = admission == currentAdmission
      )
    }
    SectionBottomSpacer()
  }
}

private val memberCriterias: List<Pair<MemberCriteria?, String>> = listOf(
  null to generalGetString(MR.strings.member_criteria_off),
  MemberCriteria.All to generalGetString(MR.strings.member_criteria_all)
)

@Composable
private fun AdmissionSection(
  admissionStageStrId: StringResource,
  admissionStageDescrStrId: StringResource,
  memberCriteria: State<MemberCriteria?>,
  groupInfo: GroupInfo,
  onSelected: (MemberCriteria?) -> Unit
) {
  SectionView {
    if (groupInfo.isOwner) {
      ExposedDropDownSettingRow(
        generalGetString(admissionStageStrId),
        memberCriterias,
        memberCriteria,
        onSelected = { value ->
          onSelected(value)
        }
      )
    } else {
      InfoRow(
        stringResource(admissionStageStrId),
        memberCriteria.value?.text ?: generalGetString(MR.strings.member_criteria_off)
      )
    }
  }
  SectionTextFooter(stringResource( admissionStageDescrStrId))
}

@Composable
private fun ResetSaveButtons(reset: () -> Unit, save: () -> Unit, disabled: Boolean) {
  SectionView {
    SectionItemView(reset, disabled = disabled) {
      Text(stringResource(MR.strings.reset_verb), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
    SectionItemView(save, disabled = disabled) {
      Text(stringResource(MR.strings.save_and_notify_group_members), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
    }
  }
}

private fun showUnsavedChangesAlert(save: () -> Unit, revert: () -> Unit) {
  AlertManager.shared.showAlertDialogStacked(
    title = generalGetString(MR.strings.save_admission_question),
    confirmText = generalGetString(MR.strings.save_and_notify_group_members),
    dismissText = generalGetString(MR.strings.exit_without_saving),
    onConfirm = save,
    onDismiss = revert,
  )
}
