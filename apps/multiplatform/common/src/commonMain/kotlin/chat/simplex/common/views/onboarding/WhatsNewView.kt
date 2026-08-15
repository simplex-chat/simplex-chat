package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.gestures.awaitEachGesture
import androidx.compose.foundation.gestures.awaitFirstDown
import androidx.compose.foundation.gestures.calculatePan
import androidx.compose.foundation.gestures.calculateZoom
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.graphicsLayer
import androidx.compose.ui.input.pointer.PointerEventPass
import androidx.compose.ui.input.pointer.PointerIcon
import androidx.compose.ui.input.pointer.pointerHoverIcon
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.layout.onGloballyPositioned
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.LinkAnnotation
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.text.withLink
import androidx.compose.ui.text.withStyle
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.setConditionsNotified
import chat.simplex.common.model.ServerOperator.Companion.dummyOperatorInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chat.item.CIFileViewScope
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.UserAddressView
import chat.simplex.common.views.usersettings.networkAndServers.UsageConditionsView
import chat.simplex.common.views.usersettings.showAddShortLinkAlert
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource
import kotlin.math.absoluteValue

@Composable
fun ModalData.WhatsNewView(updatedConditions: Boolean = false, viaSettings: Boolean = false, close: () -> Unit) {
  val currentVersion = remember { mutableStateOf(versionDescriptions.lastIndex) }
  val rhId = chatModel.remoteHostId()

  if (updatedConditions) {
    LaunchedEffect(Unit) {
      val conditionsId = chatModel.conditions.value.currentConditions.conditionsId
      try {
        setConditionsNotified(rh = rhId, conditionsId = conditionsId)
      } catch (e: Exception) {
        Log.d(TAG, "WhatsNewView setConditionsNotified error: ${e.message}")
      }
    }
  }

  @Composable
  fun featureDescription(icon: ImageResource?, titleId: StringResource, descrId: StringResource?, link: String?, subfeatures: List<Pair<ImageResource, StringResource>>) {
    @Composable
    fun linkButton(link: String) {
      val uriHandler = LocalUriHandler.current
      Icon(
        painterResource(MR.images.ic_open_in_new), stringResource(titleId), tint = MaterialTheme.colors.primary,
        modifier = Modifier
          .clickable { if (link.startsWith("simplex:")) uriHandler.openVerifiedSimplexUri(link) else uriHandler.openExternalLink(link) }
      )
    }

    Column(modifier = Modifier.padding(bottom = 12.dp)) {
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp),
        modifier = Modifier.padding(bottom = 4.dp)
      ) {
        if (icon != null)  Icon(painterResource(icon), stringResource(titleId), tint = MaterialTheme.colors.secondary)
        Text(
          generalGetString(titleId),
          maxLines = 2,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.h4,
          fontWeight = FontWeight.Medium,
          modifier = Modifier.padding(bottom = 6.dp)
        )
        if (link != null) {
          linkButton(link)
        }
      }
      if (descrId != null) Text(generalGetString(descrId), fontSize = 15.sp)
      for ((si, sd) in subfeatures) {
        Row(
          verticalAlignment = Alignment.CenterVertically,
          horizontalArrangement = Arrangement.spacedBy(8.dp),
          modifier = Modifier.padding(bottom = 6.dp)
        ) {
          Icon(painterResource(si), stringResource(sd), tint = MaterialTheme.colors.secondary)
          Text(generalGetString(sd), fontSize = 15.sp)
        }
      }
    }
  }

  @Composable
  fun pagination() {
    Row(
      Modifier
        .padding(bottom = DEFAULT_PADDING)
    ) {
      if (currentVersion.value > 0) {
        val prev = currentVersion.value - 1
        Box(Modifier.clip(RoundedCornerShape(20.dp))) {
          Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier
              .clickable { currentVersion.value = prev }
              .padding(8.dp)
          ) {
            Icon(painterResource(MR.images.ic_arrow_back_ios_new), "previous", tint = MaterialTheme.colors.primary)
            Text(versionDescriptions[prev].version, color = MaterialTheme.colors.primary)
          }
        }
      }
      Spacer(Modifier.fillMaxWidth().weight(1f))
      if (currentVersion.value < versionDescriptions.lastIndex) {
        val next = currentVersion.value + 1
        Box(Modifier.clip(RoundedCornerShape(20.dp))) {
          Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier
              .clickable { currentVersion.value = next }
              .padding(8.dp)
          ) {
            Text(versionDescriptions[next].version, color = MaterialTheme.colors.primary)
            Icon(painterResource(MR.images.ic_arrow_forward_ios), "next", tint = MaterialTheme.colors.primary)
          }
        }
      }
    }
  }

  val v = versionDescriptions[currentVersion.value]

  ModalView(close = close) {
    ColumnWithScrollBar(
      Modifier
        .padding(horizontal = DEFAULT_PADDING),
      verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING.times(0.75f))
    ) {
      AppBarTitle(String.format(generalGetString(MR.strings.new_in_version), v.version), withPadding = false, bottomPadding = DEFAULT_PADDING)

      val modalManager = if (viaSettings) ModalManager.start else ModalManager.center

      v.features.forEach { feature ->
        when (feature) {
          is VersionFeature.FeatureDescription -> {
            if (feature.show) {
              featureDescription(feature.icon, feature.titleId, feature.descrId, feature.link, feature.subfeatures)
            }
          }
          is VersionFeature.FeatureView -> {
            feature.view(modalManager)
          }
        }
      }

      if (v.post != null) {
        ReadMoreButton(v.post)
      }

      if (updatedConditions) {
        Text(
          stringResource(MR.strings.view_updated_conditions),
          color = MaterialTheme.colors.primary,
          modifier = Modifier
            .clickable(
              interactionSource = remember { MutableInteractionSource() },
              indication = null
            ) {
              modalManager.showModalCloseable { close ->
                UsageConditionsView(
                  userServers = mutableStateOf(emptyList()),
                  currUserServers = mutableStateOf(emptyList()),
                  close = close,
                  rhId = rhId
                )
              }
            }
        )
      }

      if (!viaSettings) {
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Box(
          Modifier.fillMaxWidth(), contentAlignment = Alignment.Center
        ) {
          Box(Modifier.clip(RoundedCornerShape(20.dp))) {
            Row(
              verticalAlignment = Alignment.CenterVertically,
              horizontalArrangement = Arrangement.Center,
              modifier = Modifier
                .clickable { close() }
                .padding(8.dp)
            ) {
              Text(
                generalGetString(MR.strings.ok),
                style = MaterialTheme.typography.h3,
                color = MaterialTheme.colors.primary
              )
            }
          }
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
      }

      Spacer(Modifier.fillMaxHeight().weight(1f))

      pagination()
    }
  }
}

@Composable
fun ReadMoreButton(url: String) {
  val uriHandler = LocalUriHandler.current
  Row(horizontalArrangement = Arrangement.spacedBy(8.dp), modifier = Modifier.padding(top = DEFAULT_PADDING.div(4))) {
    Text(
      stringResource(MR.strings.whats_new_read_more),
      color = MaterialTheme.colors.primary,
      modifier = Modifier
        .clickable(
          interactionSource = remember { MutableInteractionSource() },
          indication = null
        ) {
          uriHandler.openExternalLink(url)
        }
    )
    Icon(painterResource(MR.images.ic_open_in_new), stringResource(MR.strings.whats_new_read_more), tint = MaterialTheme.colors.primary)
  }
}

private sealed class VersionFeature {
  class FeatureDescription(
    val icon: ImageResource?,
    val titleId: StringResource,
    val descrId: StringResource?,
    var subfeatures: List<Pair<ImageResource, StringResource>> = listOf(),
    val link: String? = null,
    val show: Boolean = true
  ): VersionFeature()

  class FeatureView(
    val icon: ImageResource?,
    val titleId: StringResource,
    val view: @Composable (modalManager: ModalManager) -> Unit
  ): VersionFeature()
}

private data class VersionDescription(
  val version: String,
  val features: List<VersionFeature>,
  val post: String? = null,
)

private val versionDescriptions: List<VersionDescription> = listOf(
  VersionDescription(
    version = "v4.2",
    post = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v4_2_security_assessment,
        descrId = MR.strings.v4_2_security_assessment_desc,
        link = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_group,
        titleId = MR.strings.v4_2_group_links,
        descrId = MR.strings.v4_2_group_links_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_check,
        titleId = MR.strings.v4_2_auto_accept_contact_requests,
        descrId = MR.strings.v4_2_auto_accept_contact_requests_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.3",
    post = "https://simplex.chat/blog/20221206-simplex-chat-v4.3-voice-messages.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_mic,
        titleId = MR.strings.v4_3_voice_messages,
        descrId = MR.strings.v4_3_voice_messages_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_delete_forever,
        titleId = MR.strings.v4_3_irreversible_message_deletion,
        descrId = MR.strings.v4_3_irreversible_message_deletion_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_wifi_tethering,
        titleId = MR.strings.v4_3_improved_server_configuration,
        descrId = MR.strings.v4_3_improved_server_configuration_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_visibility_off,
        titleId = MR.strings.v4_3_improved_privacy_and_security,
        descrId = MR.strings.v4_3_improved_privacy_and_security_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.4",
    post = "https://simplex.chat/blog/20230103-simplex-chat-v4.4-disappearing-messages.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_timer,
        titleId = MR.strings.v4_4_disappearing_messages,
        descrId = MR.strings.v4_4_disappearing_messages_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_pending,
        titleId = MR.strings.v4_4_live_messages,
        descrId = MR.strings.v4_4_live_messages_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v4_4_verify_connection_security,
        descrId = MR.strings.v4_4_verify_connection_security_desc
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v4_4_french_interface,
        descrId = MR.strings.v4_4_french_interface_descr
      )
    )
  ),
  VersionDescription(
    version = "v4.5",
    post = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_manage_accounts,
        titleId = MR.strings.v4_5_multiple_chat_profiles,
        descrId = MR.strings.v4_5_multiple_chat_profiles_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_edit_note,
        titleId = MR.strings.v4_5_message_draft,
        descrId = MR.strings.v4_5_message_draft_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_safety_divider,
        titleId = MR.strings.v4_5_transport_isolation,
        descrId = MR.strings.v4_5_transport_isolation_descr,
        link = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_task,
        titleId = MR.strings.v4_5_private_filenames,
        descrId = MR.strings.v4_5_private_filenames_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_battery_2_bar,
        titleId = MR.strings.v4_5_reduced_battery_usage,
        descrId = MR.strings.v4_5_reduced_battery_usage_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v4_5_italian_interface,
        descrId = MR.strings.v4_5_italian_interface_descr,
      )
    )
  ),
  VersionDescription(
    version = "v4.6",
    post = "https://simplex.chat/blog/20230328-simplex-chat-v4-6-hidden-profiles.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v4_6_hidden_chat_profiles,
        descrId = MR.strings.v4_6_hidden_chat_profiles_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_flag,
        titleId = MR.strings.v4_6_group_moderation,
        descrId = MR.strings.v4_6_group_moderation_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_maps_ugc,
        titleId = MR.strings.v4_6_group_welcome_message,
        descrId = MR.strings.v4_6_group_welcome_message_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_call,
        titleId = MR.strings.v4_6_audio_video_calls,
        descrId = MR.strings.v4_6_audio_video_calls_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v4_6_reduced_battery_usage,
        descrId = MR.strings.v4_6_reduced_battery_usage_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v4_6_chinese_spanish_interface,
        descrId = MR.strings.v4_6_chinese_spanish_interface_descr,
      )
    )
  ),
  VersionDescription(
    version = "v5.0",
    post = "https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_upload_file,
        titleId = MR.strings.v5_0_large_files_support,
        descrId = MR.strings.v5_0_large_files_support_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_0_app_passcode,
        descrId = MR.strings.v5_0_app_passcode_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_0_polish_interface,
        descrId = MR.strings.v5_0_polish_interface_descr,
      )
    )
  ),
  VersionDescription(
    version = "v5.1",
    post = "https://simplex.chat/blog/20230523-simplex-chat-v5-1-message-reactions-self-destruct-passcode.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_add_reaction,
        titleId = MR.strings.v5_1_message_reactions,
        descrId = MR.strings.v5_1_message_reactions_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_chat,
        titleId = MR.strings.v5_1_better_messages,
        descrId = MR.strings.v5_1_better_messages_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_light_mode,
        titleId = MR.strings.v5_1_custom_themes,
        descrId = MR.strings.v5_1_custom_themes_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_1_self_destruct_passcode,
        descrId = MR.strings.v5_1_self_destruct_passcode_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_1_japanese_portuguese_interface,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate,
      )
    )
  ),
  VersionDescription(
    version = "v5.2",
    post = "https://simplex.chat/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_check,
        titleId = MR.strings.v5_2_message_delivery_receipts,
        descrId = MR.strings.v5_2_message_delivery_receipts_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_star,
        titleId = MR.strings.v5_2_favourites_filter,
        descrId = MR.strings.v5_2_favourites_filter_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_sync_problem,
        titleId = MR.strings.v5_2_fix_encryption,
        descrId = MR.strings.v5_2_fix_encryption_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_timer,
        titleId = MR.strings.v5_2_disappear_one_message,
        descrId = MR.strings.v5_2_disappear_one_message_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_redeem,
        titleId = MR.strings.v5_2_more_things,
        descrId = MR.strings.v5_2_more_things_descr
      )
    )
  ),
  VersionDescription(
    version = "v5.3",
    post = "https://simplex.chat/blog/20230925-simplex-chat-v5-3-desktop-app-local-file-encryption-directory-service.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_desktop,
        titleId = MR.strings.v5_3_new_desktop_app,
        descrId = MR.strings.v5_3_new_desktop_app_descr,
        link = "https://simplex.chat/downloads/"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_3_encrypt_local_files,
        descrId = MR.strings.v5_3_encrypt_local_files_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_search,
        titleId = MR.strings.v5_3_discover_join_groups,
        descrId = MR.strings.v5_3_discover_join_groups_descr,
        link = "simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_theater_comedy,
        titleId = MR.strings.v5_3_simpler_incognito_mode,
        descrId = MR.strings.v5_3_simpler_incognito_mode_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_3_new_interface_languages,
        descrId = MR.strings.v5_3_new_interface_languages_descr,
      )
    )
  ),
  VersionDescription(
    version = "v5.4",
    post = "https://simplex.chat/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_desktop,
        titleId = MR.strings.v5_4_link_mobile_desktop,
        descrId = MR.strings.v5_4_link_mobile_desktop_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_group,
        titleId = MR.strings.v5_4_better_groups,
        descrId = MR.strings.v5_4_better_groups_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_theater_comedy,
        titleId = MR.strings.v5_4_incognito_groups,
        descrId = MR.strings.v5_4_incognito_groups_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_back_hand,
        titleId = MR.strings.v5_4_block_group_members,
        descrId = MR.strings.v5_4_block_group_members_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_redeem,
        titleId = MR.strings.v5_2_more_things,
        descrId = MR.strings.v5_4_more_things_descr
      )
    )
  ),
  VersionDescription(
    version = "v5.5",
    post = "https://simplex.chat/blog/20240124-simplex-chat-infrastructure-costs-v5-5-simplex-ux-private-notes-group-history.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_folder_pen,
        titleId = MR.strings.v5_5_private_notes,
        descrId = MR.strings.v5_5_private_notes_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_link,
        titleId = MR.strings.v5_5_simpler_connect_ui,
        descrId = MR.strings.v5_5_simpler_connect_ui_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_forum,
        titleId = MR.strings.v5_5_join_group_conversation,
        descrId = MR.strings.v5_5_join_group_conversation_descr,
        link = "simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v5_5_message_delivery,
        descrId = MR.strings.v5_5_message_delivery_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_5_new_interface_languages,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate,
      )
    )
  ),
  VersionDescription(
    version = "v5.6",
    post = "https://simplex.chat/blog/20240323-simplex-network-privacy-non-profit-v5-6-quantum-resistant-e2e-encryption-simple-migration.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_vpn_key_filled,
        titleId = MR.strings.v5_6_quantum_resistant_encryption,
        descrId = MR.strings.v5_6_quantum_resistant_encryption_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_ios_share,
        titleId = MR.strings.v5_6_app_data_migration,
        descrId = MR.strings.v5_6_app_data_migration_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_call,
        titleId = MR.strings.v5_6_picture_in_picture_calls,
        descrId = MR.strings.v5_6_picture_in_picture_calls_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_back_hand,
        titleId = MR.strings.v5_6_safer_groups,
        descrId = MR.strings.v5_6_safer_groups_descr
      )
    )
  ),
  VersionDescription(
    version = "v5.7",
    post = "https://simplex.chat/blog/20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_vpn_key_filled,
        titleId = MR.strings.v5_6_quantum_resistant_encryption,
        descrId = MR.strings.v5_7_quantum_resistant_encryption_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_forward,
        titleId = MR.strings.v5_7_forward,
        descrId = MR.strings.v5_7_forward_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_music_note,
        titleId = MR.strings.v5_7_call_sounds,
        descrId = MR.strings.v5_7_call_sounds_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_account_box,
        titleId = MR.strings.v5_7_shape_profile_images,
        descrId = MR.strings.v5_7_shape_profile_images_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_wifi_tethering,
        titleId = MR.strings.v5_7_network,
        descrId = MR.strings.v5_7_network_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_7_new_interface_languages,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate,
      )
    )
  ),
  VersionDescription(
    version = "v5.8",
    post = "https://simplex.chat/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_settings_ethernet,
        titleId = MR.strings.v5_8_private_routing,
        descrId = MR.strings.v5_8_private_routing_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_palette,
        titleId = MR.strings.v5_8_chat_themes,
        descrId = MR.strings.v5_8_chat_themes_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_security,
        titleId = MR.strings.v5_8_safe_files,
        descrId = MR.strings.v5_8_safe_files_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v5_8_message_delivery,
        descrId = MR.strings.v5_8_message_delivery_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_8_persian_ui,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate
      )
    )
  ),
  VersionDescription(
    version = "v6.0",
    post = "https://simplex.chat/blog/20240814-simplex-chat-vision-funding-v6-private-routing-new-user-experience.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_0_new_chat_experience,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_add_link to MR.strings.v6_0_connect_faster_descr,
          MR.images.ic_inventory_2 to MR.strings.v6_0_your_contacts_descr,
          MR.images.ic_delete to MR.strings.v6_0_delete_many_messages_descr,
          MR.images.ic_match_case to MR.strings.v6_0_increase_font_size
        )
      ),
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_0_new_media_options,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_play_arrow_filled to MR.strings.v6_0_chat_list_media,
          MR.images.ic_blur_on to MR.strings.v6_0_privacy_blur,
        )
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_toast,
        titleId = MR.strings.v6_0_reachable_chat_toolbar,
        descrId = MR.strings.v6_0_reachable_chat_toolbar_descr,
        show = appPlatform.isAndroid
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_settings_ethernet,
        titleId = MR.strings.v5_8_private_routing,
        descrId = MR.strings.v6_0_private_routing_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_wifi_tethering,
        titleId = MR.strings.v6_0_connection_servers_status,
        descrId = MR.strings.v6_0_connection_servers_status_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_upgrade,
        titleId = MR.strings.v6_0_upgrade_app,
        descrId = MR.strings.v6_0_upgrade_app_descr,
        show = appPlatform.isDesktop
      ),
    ),
  ),
  VersionDescription(
    version = "v6.1",
    post = "https://simplex.chat/blog/20241014-simplex-network-v6-1-security-review-better-calls-user-experience.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v6_1_better_security,
        descrId = MR.strings.v6_1_better_security_descr,
        link = "https://simplex.chat/blog/20241014-simplex-network-v6-1-security-review-better-calls-user-experience.html"
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_videocam,
        titleId = MR.strings.v6_1_better_calls,
        descrId = MR.strings.v6_1_better_calls_descr
      ),
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_1_better_user_experience,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_link to MR.strings.v6_1_switch_chat_profile_descr,
          MR.images.ic_chat to MR.strings.v6_1_customizable_message_descr,
          MR.images.ic_calendar to MR.strings.v6_1_message_dates_descr,
          MR.images.ic_forward to MR.strings.v6_1_forward_many_messages_descr,
          MR.images.ic_delete to MR.strings.v6_1_delete_many_messages_descr
        )
      ),
    ),
  ),
  VersionDescription(
    version = "v6.2",
    post = "https://simplex.chat/blog/20241210-simplex-network-v6-2-servers-by-flux-business-chats.html",
    features = listOf(
      VersionFeature.FeatureView(
        icon = null,
        titleId = MR.strings.v6_2_network_decentralization,
        view = { modalManager ->
          Column {
            val src = (operatorsInfo[OperatorTag.Flux] ?: dummyOperatorInfo).largeLogo
            Image(painterResource(src), null, modifier = Modifier.height(48.dp))
            Text(stringResource(MR.strings.v6_2_network_decentralization_descr), modifier = Modifier.padding(top = 8.dp))
            Text(stringResource(MR.strings.v6_2_network_decentralization_enable_flux))
          }
        }
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_work,
        titleId = MR.strings.v6_2_business_chats,
        descrId = MR.strings.v6_2_business_chats_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_chat,
        titleId = MR.strings.v6_2_improved_chat_navigation,
        descrId = MR.strings.v6_2_improved_chat_navigation_descr
      ),
    ),
  ),
  VersionDescription(
    version = "v6.3",
    post = "https://simplex.chat/blog/20250308-simplex-chat-v6-3-new-user-experience-safety-in-public-groups.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_at,
        titleId = MR.strings.v6_3_mentions,
        descrId = MR.strings.v6_3_mentions_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_flag,
        titleId = MR.strings.v6_3_reports,
        descrId = MR.strings.v6_3_reports_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_menu,
        titleId = MR.strings.v6_3_organize_chat_lists,
        descrId = MR.strings.v6_3_organize_chat_lists_descr
      ),
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_3_better_privacy_and_security,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_visibility_off to MR.strings.v6_3_private_media_file_names,
          MR.images.ic_delete to MR.strings.v6_3_set_message_expiration_in_chats
        )
      ),
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_3_better_groups_performance,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_bolt to MR.strings.v6_3_faster_sending_messages,
          MR.images.ic_group_off to MR.strings.v6_3_faster_deletion_of_groups
        )
      ),
    )
  ),
  VersionDescription(
    version = "v6.4",
    post = "https://simplex.chat/blog/20250703-simplex-network-protocol-extension-for-securely-connecting-people.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_person,
        titleId = MR.strings.v6_4_connect_faster,
        descrId = MR.strings.v6_4_connect_faster_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_chat_person,
        titleId = MR.strings.v6_4_review_members,
        descrId = MR.strings.v6_4_review_members_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_contact_support,
        titleId = MR.strings.v6_4_support_chat,
        descrId = MR.strings.v6_4_support_chat_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_flag,
        titleId = MR.strings.v6_4_role_moderator,
        descrId = MR.strings.v6_4_role_moderator_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v5_8_message_delivery,
        descrId = MR.strings.v6_4_message_delivery_descr
      ),
    )
  ),
  VersionDescription(
    version = "v6.4.1",
    post = "https://simplex.chat/blog/20250729-simplex-chat-v6-4-1-welcome-contacts-protect-groups-app-security.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_waving_hand,
        titleId = MR.strings.v6_4_1_welcome_contacts,
        descrId = MR.strings.v6_4_1_welcome_contacts_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_timer,
        titleId = MR.strings.v6_4_1_keep_chats_clean,
        descrId = MR.strings.v6_4_1_keep_chats_clean_descr
      ),
      VersionFeature.FeatureView(
        icon = null,
        titleId = MR.strings.v6_4_1_short_address,
        view = { modalManager -> CreateUpdateAddressShortLinkView(modalManager) }
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v6_4_1_new_interface_languages,
        descrId = MR.strings.v6_4_1_new_interface_languages_descr,
      ),
    )
  ),
  VersionDescription(
    version = "v6.5",
    post = "https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html",
    features = listOf(
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v6_5_public_channels,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_wifi_tethering to MR.strings.v6_5_reliability,
          MR.images.ic_dns to MR.strings.v6_5_ownership,
          MR.images.ic_vpn_key_filled to MR.strings.v6_5_security,
          MR.images.ic_shield to MR.strings.v6_5_privacy,
        )
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_add_link,
        titleId = MR.strings.v6_5_invite_friends,
        descrId = MR.strings.v6_5_invite_friends_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_security,
        titleId = MR.strings.v6_5_safe_web_links,
        descrId = MR.strings.v6_5_safe_web_links_descr
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v6_5_non_profit_governance,
        descrId = MR.strings.v6_5_non_profit_governance_descr
      ),
    )
  ),
  VersionDescription(
    // the trailing space differs from the previously released "v7.0", so that What's new is shown again
    version = "v7.0 ",
    post = null,
    features = listOf(
      VersionFeature.FeatureView(
        icon = null,
        titleId = MR.strings.v7_0_invest,
        view = { modalManager -> InvestInSimpleXChatView(modalManager) }
      ),
      VersionFeature.FeatureDescription(
        icon = MR.images.ic_alternate_email,
        titleId = MR.strings.v7_0_simplex_names,
        descrId = MR.strings.v7_0_simplex_names_descr
      ),
      VersionFeature.FeatureDescription(
        icon = null,
        titleId = MR.strings.v7_0_channels,
        descrId = null,
        subfeatures = listOf(
          MR.images.ic_person_add to MR.strings.v7_0_channels_contributors,
          MR.images.ic_travel_explore to MR.strings.v7_0_channels_previews,
          MR.images.ic_dns to MR.strings.v7_0_channels_relays,
          MR.images.ic_article to MR.strings.v7_0_channels_wider_messages,
        )
      ),
    )
  ),
)

private val lastVersion = versionDescriptions.last().version

fun setLastVersionDefault(m: ChatModel) {
  if (appPrefs.whatsNewVersion.get() != lastVersion) {
    appPrefs.whatsNewVersion.set(lastVersion)
  }
}

fun shouldShowWhatsNew(m: ChatModel): Boolean {
  val v = m.controller.appPrefs.whatsNewVersion.get()
  setLastVersionDefault(m)
  return v != lastVersion
}

private const val WEFUNDER_URL = "https://wefunder.com/simplex.chat?utm_source=app"

private const val CROWDFUNDING_CONTACT_URI = "simplex:/a#JxGcOA1_QhlmVFzYYabloMbvMZk5Y9d9iS3ITDnhzYo?h=smp11.simplex.im"

// the center modal takes the remaining width of the window, so the image is limited to its design width
private val MAX_CROWDFUNDING_IMAGE_WIDTH = DEFAULT_MIN_CENTER_MODAL_WIDTH

// Google Play policy restricts promoting investments, so Play builds only show it in the US
@Composable
fun crowdfundingAvailable(): Boolean {
  if (!platform.androidIsPlayStoreBuild) return true
  LaunchedEffect(Unit) {
    if (androidPlayStoreCountry.value == null) platform.androidLoadPlayStoreCountry()
  }
  return androidPlayStoreCountry.value == "US"
}

@Composable
private fun InvestInSimpleXChatView(modalManager: ModalManager) {
  if (!crowdfundingAvailable()) return
  val showGetStake = { modalManager.showModalCloseable(cardScreen = true) { close -> GetStakeView(close) } }
  Column(modifier = Modifier.padding(bottom = 12.dp)) {
    Text(
      generalGetString(MR.strings.v7_0_invest),
      style = MaterialTheme.typography.h4,
      fontWeight = FontWeight.Medium,
      modifier = Modifier.padding(bottom = 6.dp)
    )
    Text(
      buildAnnotatedString {
        append(generalGetString(MR.strings.v7_0_invest_descr))
        append(" ")
        withStyle(SpanStyle(color = MaterialTheme.colors.primary)) {
          append(generalGetString(MR.strings.learn_more))
        }
      },
      fontSize = 15.sp,
      modifier = Modifier
        .pointerHoverIcon(PointerIcon.Hand)
        .clickable(
          interactionSource = remember { MutableInteractionSource() },
          indication = null,
          onClick = showGetStake
        )
    )
    if (BuildConfigCommon.SIMPLEX_ASSETS) {
      Image(
        painterResource(MR.images.crowdfunding_00),
        contentDescription = null,
        contentScale = ContentScale.FillWidth,
        modifier = Modifier
          .padding(top = 8.dp)
          .widthIn(max = MAX_CROWDFUNDING_IMAGE_WIDTH)
          .fillMaxWidth()
          .clip(RoundedCornerShape(12.dp))
          .pointerHoverIcon(PointerIcon.Hand)
          .clickable(
            interactionSource = remember { MutableInteractionSource() },
            indication = null,
            onClick = showGetStake
          )
      )
    }
  }
}

private class CrowdfundingSlide(
  val image: ImageResource,
  val heading: String,
  val info: String?,
  val text: String,
)

// not localized: the page is only shown to US investors, and the text duplicates the images
private val getStakeSlides: List<CrowdfundingSlide> = listOf(
  CrowdfundingSlide(
    MR.images.crowdfunding_00,
    "The first and the only messaging network without any user IDs",
    null,
    "SimpleX users have been more than doubling every year – and you can now acquire a stake in the company that builds it."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_04,
    "480,000+ users joined on their own",
    null,
    "All these users found SimpleX Chat without any paid marketing – and donated over \$650,000, paying for something they could use for free."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_05,
    "SimpleX is a network, not an app",
    "Users, creators, businesses, developers and operators all arrived organically – the cold start solved.",
    "Each group of users makes the network more valuable to the rest, driving organic growth."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_06,
    "Developers already bet on SimpleX success",
    "Independent developers created moderation and AI bots, Telegram bridges, and a public server registry.",
    "Some projects describe themselves as SimpleX-first, running all communications of their applications over SimpleX Network."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_07,
    "Why SimpleX can't be copied",
    "Only SimpleX combines scalable delivery, ownership that can't be revoked, and participants that can't be identified.",
    "Other networks rely on user IDs to deliver messages, and large platforms monetize them. Removing IDs would require rebuilding."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_10,
    "An open network others can build on",
    "The SimpleX Network Consortium agreement ensures that no single company controls the network, while protecting SimpleX Chat business.",
    "The protocol is licensed to the foundation permanently – the network remains available regardless of who owns the company."
  ),
  CrowdfundingSlide(
    MR.images.crowdfunding_11,
    "We are building a network that people own",
    "We invite you to invest and become part of it.",
    "Read about how we plan to make SimpleX Chat and network profitable, and about all the investment terms on Wefunder."
  ),
)

@Composable
fun GetStakeView(close: () -> Unit) {
  val uriHandler = LocalUriHandler.current
  val stopped = chatModel.chatRunning.value == false
  ColumnWithScrollBar(Modifier.pinchZoom().padding(horizontal = DEFAULT_PADDING)) {
    AppBarTitle("Get a stake in\nSimpleX Chat", withPadding = false)
    Text(
      buildAnnotatedString {
        append("By investing, you can benefit from the company growth, and help us build the future of private and secure communications.")
        // only the link is clickable, the rest of the paragraph is not
        withLink(LinkAnnotation.Url(WEFUNDER_URL) { uriHandler.openExternalLink(WEFUNDER_URL) }) {
          withStyle(SpanStyle(color = MaterialTheme.colors.primary, fontWeight = FontWeight.Bold)) {
            append(" Learn more and invest on Wefunder.")
          }
        }
      },
      lineHeight = 24.sp
    )

    getStakeSlides.forEach { slide ->
      Column(Modifier.padding(top = DEFAULT_PADDING * 1.5f)) {
        if (BuildConfigCommon.SIMPLEX_ASSETS) {
          Image(
            painterResource(slide.image),
            contentDescription = null,
            contentScale = ContentScale.FillWidth,
            modifier = Modifier.fillMaxWidth().clip(RoundedCornerShape(12.dp)).fullScreenOnClick(slide.image)
          )
        } else {
          Text(slide.heading, style = MaterialTheme.typography.h4, fontWeight = FontWeight.Medium)
          if (slide.info != null) {
            Text(slide.info, Modifier.padding(top = 4.dp), lineHeight = 24.sp)
          }
        }
        Text(slide.text, Modifier.padding(top = 8.dp), lineHeight = 24.sp)
      }
    }

    Column(
      Modifier.fillMaxWidth().padding(top = DEFAULT_PADDING * 2),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      OnboardingActionButton(
        if (appPlatform.isAndroid) Modifier.fillMaxWidth() else Modifier.widthIn(min = 300.dp),
        labelId = MR.strings.v7_0_invest_learn_more,
        onboarding = null,
        onclick = { uriHandler.openExternalLink(WEFUNDER_URL) }
      )
      if (!chatModel.desktopNoUserNoRemote) {
        TextButtonBelowOnboardingButton(
          "or ask SimpleX team",
          onClick = if (stopped) null else ({
            close()
            uriHandler.openVerifiedSimplexUri(CROWDFUNDING_CONTACT_URI)
          })
        )
      }
    }
  }
}

// there is no pinch gesture with a mouse, so on desktop a slide is opened full screen instead
@Composable
private fun Modifier.fullScreenOnClick(image: ImageResource): Modifier {
  if (!appPlatform.isDesktop) return this
  return pointerHoverIcon(PointerIcon.Hand).clickable(
    interactionSource = remember { MutableInteractionSource() },
    indication = null
  ) {
    ModalManager.fullscreen.showCustomModal { close ->
      BackHandler(onBack = close)
      Box(
        Modifier
          .fillMaxSize()
          .background(Color.Black)
          .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = close),
        contentAlignment = Alignment.Center
      ) {
        Image(painterResource(image), contentDescription = null, contentScale = ContentScale.Fit, modifier = Modifier.fillMaxSize())
      }
    }
  }
}

private const val MAX_PAGE_ZOOM = 5f

/**
 * The slide images contain small text that is unreadable at screen width, so the page can be pinch-zoomed.
 * Android only: pinch is unavailable with a mouse.
 */
@Composable
private fun Modifier.pinchZoom(): Modifier {
  if (!appPlatform.isAndroid) return this
  var scale by remember { mutableStateOf(1f) }
  var offsetX by remember { mutableStateOf(0f) }
  var offsetY by remember { mutableStateOf(0f) }
  var size by remember { mutableStateOf(IntSize.Zero) }
  return this
    .onGloballyPositioned { size = it.size }
    .graphicsLayer {
      scaleX = scale
      scaleY = scale
      translationX = offsetX
      translationY = offsetY
    }
    .pointerInput(Unit) {
      awaitEachGesture {
        // the initial pass, as the scroll of the same column is applied after this modifier and would take the gesture first
        awaitFirstDown(requireUnconsumed = false, pass = PointerEventPass.Initial)
        var taken: Boolean? = null
        do {
          val event = awaitPointerEvent(PointerEventPass.Initial)
          val multiTouch = event.changes.count { it.pressed } > 1
          if (multiTouch || scale > 1f) {
            scale = (scale * event.calculateZoom()).coerceIn(1f, MAX_PAGE_ZOOM)
            val pan = event.calculatePan()
            // the page is scaled around its center, so it can be panned by half of the overflow in each direction
            val maxX = size.width * (scale - 1f) / 2
            val maxY = size.height * (scale - 1f) / 2
            val pannedY = offsetY + pan.y * scale
            // the clamp is applied even when the gesture is not taken: at scale 1 both bounds
            // are 0, which resets the offsets after zooming back out
            offsetX = (offsetX + pan.x * scale).coerceIn(-maxX, maxX)
            offsetY = pannedY.coerceIn(-maxY, maxY)
            // two fingers always mean zoom, taken without a touch slop: waiting for one would let
            // the scroll reach its own slop first and scroll the page. A one finger drag is left
            // to the scroll at the edges, decided once so it cannot alternate mid drag
            if (multiTouch) taken = true
            else if (taken == null && pan.y != 0f) taken = pannedY.absoluteValue < maxY
            if (taken == true) event.changes.forEach { if (it.pressed) it.consume() }
          }
        } while (event.changes.any { it.pressed })
      }
    }
}

@Composable
fun CreateUpdateAddressShortLinkView(modalManager: ModalManager) {
  val clipboard = LocalClipboardManager.current
  val progressIndicator = remember { mutableStateOf(false) }

  fun share(userAddress: String) { clipboard.shareText(userAddress) }

  Column(modifier = Modifier.padding(bottom = 12.dp)) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(8.dp),
      modifier = Modifier.padding(bottom = 4.dp)
    ) {
      Icon(painterResource(MR.images.ic_link), stringResource(MR.strings.v6_4_1_short_address), tint = MaterialTheme.colors.secondary)
      Text(
        generalGetString(MR.strings.v6_4_1_short_address),
        maxLines = 2,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h4,
        fontWeight = FontWeight.Medium,
        modifier = Modifier.padding(bottom = 6.dp)
      )
    }
    val addr = chatModel.userAddress.value
    if (addr != null) {
      if (addr.shouldBeUpgraded) {
        Row(
          verticalAlignment = Alignment.CenterVertically,
          horizontalArrangement = Arrangement.spacedBy(8.dp)
        ) {
          Text(
            stringResource(MR.strings.v6_4_1_short_address_update),
            color = MaterialTheme.colors.primary,
            fontSize = 15.sp,
            modifier = Modifier
              .clickable(
                interactionSource = remember { MutableInteractionSource() },
                indication = null
              ) {
                showAddShortLinkAlert(progressIndicator = progressIndicator, share = ::share)
              }
          )
          if (progressIndicator.value) {
            CIFileViewScope.progressIndicator(sizeMultiplier = 0.5f)
          }
        }
      } else {
        Text(
          stringResource(MR.strings.v6_4_1_short_address_share),
          color = MaterialTheme.colors.primary,
          fontSize = 15.sp,
          modifier = Modifier
            .clickable(
              interactionSource = remember { MutableInteractionSource() },
              indication = null
            ) {
              share(addr.connLinkContact.simplexChatUri(short = true))
            }
        )
      }
    } else {
      Text(
        stringResource(MR.strings.v6_4_1_short_address_create),
        color = MaterialTheme.colors.primary,
        fontSize = 15.sp,
        modifier = Modifier
          .clickable(
            interactionSource = remember { MutableInteractionSource() },
            indication = null
          ) {
            modalManager.showModalCloseable { close ->
              UserAddressView(chatModel = chatModel, shareViaProfile = false, autoCreateAddress = true, close = close)
            }
          }
      )
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewWhatsNewView() {
  SimpleXTheme {
    val data = remember { ModalData() }
    data.WhatsNewView(
      viaSettings = true,
      close = {}
    )
  }
}
