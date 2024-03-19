package chat.simplex.common.views.onboarding

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.ImageResource
import dev.icerock.moko.resources.StringResource

@Composable
fun WhatsNewView(viaSettings: Boolean = false, close: () -> Unit) {
  val currentVersion = remember { mutableStateOf(versionDescriptions.lastIndex) }

  @Composable
  fun featureDescription(icon: Painter, titleId: StringResource, descrId: StringResource, link: String?) {
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
        Icon(icon, stringResource(titleId), tint = MaterialTheme.colors.secondary)
        Text(
          generalGetString(titleId),
          maxLines = 2,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.h4,
          fontWeight = FontWeight.Medium
        )
        if (link != null) {
          linkButton(link)
        }
      }
      Text(generalGetString(descrId), fontSize = 15.sp)
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
    Column(
      Modifier
        .fillMaxSize()
        .padding(horizontal = DEFAULT_PADDING)
        .verticalScroll(rememberScrollState()),
      verticalArrangement = Arrangement.spacedBy(DEFAULT_PADDING.times(0.75f))
    ) {
      AppBarTitle(String.format(generalGetString(MR.strings.new_in_version), v.version), bottomPadding = DEFAULT_PADDING)

      v.features.forEach { feature ->
        featureDescription(painterResource(feature.icon), feature.titleId, feature.descrId, feature.link)
      }

      val uriHandler = LocalUriHandler.current
      if (v.post != null) {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp), modifier = Modifier.padding(top = DEFAULT_PADDING.div(4))) {
          Text(stringResource(MR.strings.whats_new_read_more), color = MaterialTheme.colors.primary,
            modifier = Modifier.clickable { uriHandler.openUriCatching(v.post) })
          Icon(painterResource(MR.images.ic_open_in_new), stringResource(MR.strings.whats_new_read_more), tint = MaterialTheme.colors.primary)
        }
      }

      if (!viaSettings) {
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Box(
          Modifier.fillMaxWidth(), contentAlignment = Alignment.Center
        ) {
          Text(
            generalGetString(MR.strings.ok),
            modifier = Modifier.clickable(onClick = close),
            style = MaterialTheme.typography.h3,
            color = MaterialTheme.colors.primary
          )
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
      }

      Spacer(Modifier.fillMaxHeight().weight(1f))

      pagination()
    }
  }
}

private data class FeatureDescription(
  val icon: ImageResource,
  val titleId: StringResource,
  val descrId: StringResource,
  val link: String? = null
)

private data class VersionDescription(
  val version: String,
  val features: List<FeatureDescription>,
  val post: String? = null,
)

private val versionDescriptions: List<VersionDescription> = listOf(
  VersionDescription(
    version = "v4.2",
    post = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v4_2_security_assessment,
        descrId = MR.strings.v4_2_security_assessment_desc,
        link = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"
      ),
      FeatureDescription(
        icon = MR.images.ic_group,
        titleId = MR.strings.v4_2_group_links,
        descrId = MR.strings.v4_2_group_links_desc
      ),
      FeatureDescription(
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
      FeatureDescription(
        icon = MR.images.ic_mic,
        titleId = MR.strings.v4_3_voice_messages,
        descrId = MR.strings.v4_3_voice_messages_desc
      ),
      FeatureDescription(
        icon = MR.images.ic_delete_forever,
        titleId = MR.strings.v4_3_irreversible_message_deletion,
        descrId = MR.strings.v4_3_irreversible_message_deletion_desc
      ),
      FeatureDescription(
        icon = MR.images.ic_wifi_tethering,
        titleId = MR.strings.v4_3_improved_server_configuration,
        descrId = MR.strings.v4_3_improved_server_configuration_desc
      ),
      FeatureDescription(
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
      FeatureDescription(
        icon = MR.images.ic_timer,
        titleId = MR.strings.v4_4_disappearing_messages,
        descrId = MR.strings.v4_4_disappearing_messages_desc
      ),
      FeatureDescription(
        icon = MR.images.ic_pending,
        titleId = MR.strings.v4_4_live_messages,
        descrId = MR.strings.v4_4_live_messages_desc
      ),
      FeatureDescription(
        icon = MR.images.ic_verified_user,
        titleId = MR.strings.v4_4_verify_connection_security,
        descrId = MR.strings.v4_4_verify_connection_security_desc
      ),
      FeatureDescription(
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
      FeatureDescription(
        icon = MR.images.ic_manage_accounts,
        titleId = MR.strings.v4_5_multiple_chat_profiles,
        descrId = MR.strings.v4_5_multiple_chat_profiles_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_edit_note,
        titleId = MR.strings.v4_5_message_draft,
        descrId = MR.strings.v4_5_message_draft_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_safety_divider,
        titleId = MR.strings.v4_5_transport_isolation,
        descrId = MR.strings.v4_5_transport_isolation_descr,
        link = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation"
      ),
      FeatureDescription(
        icon = MR.images.ic_task,
        titleId = MR.strings.v4_5_private_filenames,
        descrId = MR.strings.v4_5_private_filenames_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_battery_2_bar,
        titleId = MR.strings.v4_5_reduced_battery_usage,
        descrId = MR.strings.v4_5_reduced_battery_usage_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v4_5_italian_interface,
        descrId = MR.strings.v4_5_italian_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v4.6",
    post = "https://simplex.chat/blog/20230328-simplex-chat-v4-6-hidden-profiles.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v4_6_hidden_chat_profiles,
        descrId = MR.strings.v4_6_hidden_chat_profiles_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_flag,
        titleId = MR.strings.v4_6_group_moderation,
        descrId = MR.strings.v4_6_group_moderation_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_maps_ugc,
        titleId = MR.strings.v4_6_group_welcome_message,
        descrId = MR.strings.v4_6_group_welcome_message_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_call,
        titleId = MR.strings.v4_6_audio_video_calls,
        descrId = MR.strings.v4_6_audio_video_calls_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v4_6_reduced_battery_usage,
        descrId = MR.strings.v4_6_reduced_battery_usage_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v4_6_chinese_spanish_interface,
        descrId = MR.strings.v4_6_chinese_spanish_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.0",
    post = "https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_upload_file,
        titleId = MR.strings.v5_0_large_files_support,
        descrId = MR.strings.v5_0_large_files_support_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_0_app_passcode,
        descrId = MR.strings.v5_0_app_passcode_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_0_polish_interface,
        descrId = MR.strings.v5_0_polish_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.1",
    post = "https://simplex.chat/blog/20230523-simplex-chat-v5-1-message-reactions-self-destruct-passcode.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_add_reaction,
        titleId = MR.strings.v5_1_message_reactions,
        descrId = MR.strings.v5_1_message_reactions_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_chat,
        titleId = MR.strings.v5_1_better_messages,
        descrId = MR.strings.v5_1_better_messages_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_light_mode,
        titleId = MR.strings.v5_1_custom_themes,
        descrId = MR.strings.v5_1_custom_themes_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_1_self_destruct_passcode,
        descrId = MR.strings.v5_1_self_destruct_passcode_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_1_japanese_portuguese_interface,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.2",
    post = "https://simplex.chat/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_check,
        titleId = MR.strings.v5_2_message_delivery_receipts,
        descrId = MR.strings.v5_2_message_delivery_receipts_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_star,
        titleId = MR.strings.v5_2_favourites_filter,
        descrId = MR.strings.v5_2_favourites_filter_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_sync_problem,
        titleId = MR.strings.v5_2_fix_encryption,
        descrId = MR.strings.v5_2_fix_encryption_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_timer,
        titleId = MR.strings.v5_2_disappear_one_message,
        descrId = MR.strings.v5_2_disappear_one_message_descr
      ),
      FeatureDescription(
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
      FeatureDescription(
        icon = MR.images.ic_desktop,
        titleId = MR.strings.v5_3_new_desktop_app,
        descrId = MR.strings.v5_3_new_desktop_app_descr,
        link = "https://simplex.chat/downloads/"
      ),
      FeatureDescription(
        icon = MR.images.ic_lock,
        titleId = MR.strings.v5_3_encrypt_local_files,
        descrId = MR.strings.v5_3_encrypt_local_files_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_search,
        titleId = MR.strings.v5_3_discover_join_groups,
        descrId = MR.strings.v5_3_discover_join_groups_descr,
        link = "simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"
      ),
      FeatureDescription(
        icon = MR.images.ic_theater_comedy,
        titleId = MR.strings.v5_3_simpler_incognito_mode,
        descrId = MR.strings.v5_3_simpler_incognito_mode_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_3_new_interface_languages,
        descrId = MR.strings.v5_3_new_interface_languages_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.4",
    post = "https://simplex.chat/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_desktop,
        titleId = MR.strings.v5_4_link_mobile_desktop,
        descrId = MR.strings.v5_4_link_mobile_desktop_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_group,
        titleId = MR.strings.v5_4_better_groups,
        descrId = MR.strings.v5_4_better_groups_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_theater_comedy,
        titleId = MR.strings.v5_4_incognito_groups,
        descrId = MR.strings.v5_4_incognito_groups_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_back_hand,
        titleId = MR.strings.v5_4_block_group_members,
        descrId = MR.strings.v5_4_block_group_members_descr
      ),
      FeatureDescription(
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
      FeatureDescription(
        icon = MR.images.ic_folder_pen,
        titleId = MR.strings.v5_5_private_notes,
        descrId = MR.strings.v5_5_private_notes_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_link,
        titleId = MR.strings.v5_5_simpler_connect_ui,
        descrId = MR.strings.v5_5_simpler_connect_ui_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_forum,
        titleId = MR.strings.v5_5_join_group_conversation,
        descrId = MR.strings.v5_5_join_group_conversation_descr,
        link = "simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"
      ),
      FeatureDescription(
        icon = MR.images.ic_battery_3_bar,
        titleId = MR.strings.v5_5_message_delivery,
        descrId = MR.strings.v5_5_message_delivery_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_translate,
        titleId = MR.strings.v5_5_new_interface_languages,
        descrId = MR.strings.whats_new_thanks_to_users_contribute_weblate,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.6",
    post = "https://simplex.chat/blog/20240323-simplex-network-privacy-non-profit-v5-6-quantum-resistant-e2e-encryption-simple-migration.html",
    features = listOf(
      FeatureDescription(
        icon = MR.images.ic_vpn_key_filled,
        titleId = MR.strings.v5_6_quantum_resistant_encryption,
        descrId = MR.strings.v5_6_quantum_resistant_encryption_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_ios_share,
        titleId = MR.strings.v5_6_app_data_migration,
        descrId = MR.strings.v5_6_app_data_migration_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_call,
        titleId = MR.strings.v5_6_picture_in_picture_calls,
        descrId = MR.strings.v5_6_picture_in_picture_calls_descr
      ),
      FeatureDescription(
        icon = MR.images.ic_back_hand,
        titleId = MR.strings.v5_6_safer_groups,
        descrId = MR.strings.v5_6_safer_groups_descr
      )
    )
  ),
)

private val lastVersion = versionDescriptions.last().version

fun setLastVersionDefault(m: ChatModel) {
  m.controller.appPrefs.whatsNewVersion.set(lastVersion)
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
    WhatsNewView(
      viaSettings = true,
      close = {}
    )
  }
}
