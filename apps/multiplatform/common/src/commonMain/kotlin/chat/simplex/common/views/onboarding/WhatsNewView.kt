package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.setConditionsNotified
import chat.simplex.common.model.ServerOperator.Companion.dummyOperatorInfo
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.UsageConditionsView
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource

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
          .clickable { if (link.startsWith("simplex:")) uriHandler.openVerifiedSimplexUri(link) else uriHandler.openUriCatching(link) }
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
          uriHandler.openUriCatching(url)
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
  )
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
