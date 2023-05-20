package chat.simplex.app.views.onboarding

import android.content.res.Configuration
import android.os.Build
import androidx.annotation.IntegerRes
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
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.BuildConfig
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun WhatsNewView(viaSettings: Boolean = false, close: () -> Unit) {
  val currentVersion = remember { mutableStateOf(versionDescriptions.lastIndex) }

  @Composable
  fun featureDescription(icon: Painter, titleId: Int, descrId: Int, link: String?) {
    @Composable
    fun linkButton(link: String) {
      val uriHandler = LocalUriHandler.current
      Icon(
        painterResource(R.drawable.ic_open_in_new), stringResource(titleId), tint = MaterialTheme.colors.primary,
        modifier = Modifier
          .clickable { uriHandler.openUriCatching(link) }
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
            Icon(painterResource(R.drawable.ic_arrow_back_ios_new), "previous", tint = MaterialTheme.colors.primary)
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
            Icon(painterResource(R.drawable.ic_arrow_forward_ios), "next", tint = MaterialTheme.colors.primary)
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
      AppBarTitle(String.format(generalGetString(R.string.new_in_version), v.version), bottomPadding = DEFAULT_PADDING)

      v.features.forEach { feature ->
        featureDescription(painterResource(feature.icon), feature.titleId, feature.descrId, feature.link)
      }

      val uriHandler = LocalUriHandler.current
      if (v.post != null) {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp), modifier = Modifier.padding(top = DEFAULT_PADDING.div(4))) {
          Text(stringResource(R.string.whats_new_read_more), color = MaterialTheme.colors.primary,
            modifier = Modifier.clickable { uriHandler.openUriCatching(v.post) })
          Icon(painterResource(R.drawable.ic_open_in_new), stringResource(R.string.whats_new_read_more), tint = MaterialTheme.colors.primary)
        }
      }

      if (!viaSettings) {
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Box(
          Modifier.fillMaxWidth(), contentAlignment = Alignment.Center
        ) {
          Text(
            generalGetString(R.string.ok),
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
  val icon: Int,
  val titleId: Int,
  val descrId: Int,
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
        icon = R.drawable.ic_verified_user,
        titleId = R.string.v4_2_security_assessment,
        descrId = R.string.v4_2_security_assessment_desc,
        link = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"
      ),
      FeatureDescription(
        icon = R.drawable.ic_group,
        titleId = R.string.v4_2_group_links,
        descrId = R.string.v4_2_group_links_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_check,
        titleId = R.string.v4_2_auto_accept_contact_requests,
        descrId = R.string.v4_2_auto_accept_contact_requests_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.3",
    post = "https://simplex.chat/blog/20221206-simplex-chat-v4.3-voice-messages.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_mic,
        titleId = R.string.v4_3_voice_messages,
        descrId = R.string.v4_3_voice_messages_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_delete_forever,
        titleId = R.string.v4_3_irreversible_message_deletion,
        descrId = R.string.v4_3_irreversible_message_deletion_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_wifi_tethering,
        titleId = R.string.v4_3_improved_server_configuration,
        descrId = R.string.v4_3_improved_server_configuration_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_visibility_off,
        titleId = R.string.v4_3_improved_privacy_and_security,
        descrId = R.string.v4_3_improved_privacy_and_security_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.4",
    post = "https://simplex.chat/blog/20230103-simplex-chat-v4.4-disappearing-messages.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_timer,
        titleId = R.string.v4_4_disappearing_messages,
        descrId = R.string.v4_4_disappearing_messages_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_pending,
        titleId = R.string.v4_4_live_messages,
        descrId = R.string.v4_4_live_messages_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_verified_user,
        titleId = R.string.v4_4_verify_connection_security,
        descrId = R.string.v4_4_verify_connection_security_desc
      ),
      FeatureDescription(
        icon = R.drawable.ic_translate,
        titleId = R.string.v4_4_french_interface,
        descrId = R.string.v4_4_french_interface_descr
      )
    )
  ),
  VersionDescription(
    version = "v4.5",
    post = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_manage_accounts,
        titleId = R.string.v4_5_multiple_chat_profiles,
        descrId = R.string.v4_5_multiple_chat_profiles_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_edit_note,
        titleId = R.string.v4_5_message_draft,
        descrId = R.string.v4_5_message_draft_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_safety_divider,
        titleId = R.string.v4_5_transport_isolation,
        descrId = R.string.v4_5_transport_isolation_descr,
        link = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation"
      ),
      FeatureDescription(
        icon = R.drawable.ic_task,
        titleId = R.string.v4_5_private_filenames,
        descrId = R.string.v4_5_private_filenames_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_battery_2_bar,
        titleId = R.string.v4_5_reduced_battery_usage,
        descrId = R.string.v4_5_reduced_battery_usage_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_translate,
        titleId = R.string.v4_5_italian_interface,
        descrId = R.string.v4_5_italian_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v4.6",
    post = "https://simplex.chat/blog/20230328-simplex-chat-v4-6-hidden-profiles.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_lock,
        titleId = R.string.v4_6_hidden_chat_profiles,
        descrId = R.string.v4_6_hidden_chat_profiles_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_flag,
        titleId = R.string.v4_6_group_moderation,
        descrId = R.string.v4_6_group_moderation_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_maps_ugc,
        titleId = R.string.v4_6_group_welcome_message,
        descrId = R.string.v4_6_group_welcome_message_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_call,
        titleId = R.string.v4_6_audio_video_calls,
        descrId = R.string.v4_6_audio_video_calls_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_battery_3_bar,
        titleId = R.string.v4_6_reduced_battery_usage,
        descrId = R.string.v4_6_reduced_battery_usage_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_translate,
        titleId = R.string.v4_6_chinese_spanish_interface,
        descrId = R.string.v4_6_chinese_spanish_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  VersionDescription(
    version = "v5.0",
    post = "https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_upload_file,
        titleId = R.string.v5_0_large_files_support,
        descrId = R.string.v5_0_large_files_support_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_lock,
        titleId = R.string.v5_0_app_passcode,
        descrId = R.string.v5_0_app_passcode_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_translate,
        titleId = R.string.v5_0_polish_interface,
        descrId = R.string.v5_0_polish_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  ),
  // Also in v5.1
  // preference to disable calls per contact
  // configurable SOCKS proxy port
  // access welcome message via a group profile
  // improve calls on lock screen
  // better formatting of times and dates
  VersionDescription(
    version = "v5.1",
    post = "https://simplex.chat/blog/20230523-simplex-chat-v5-1-message-reactions-self-destruct-passcode.html",
    features = listOf(
      FeatureDescription(
        icon = R.drawable.ic_add_reaction,
        titleId = R.string.v5_1_message_reactions,
        descrId = R.string.v5_1_message_reactions_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_chat,
        titleId = R.string.v5_1_better_messages,
        descrId = R.string.v5_1_better_messages_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_light_mode,
        titleId = R.string.v5_1_custom_themes,
        descrId = R.string.v5_1_custom_themes_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_lock,
        titleId = R.string.v5_1_self_destruct_passcode,
        descrId = R.string.v5_1_self_destruct_passcode_descr
      ),
      FeatureDescription(
        icon = R.drawable.ic_translate,
        titleId = R.string.v5_1_japanese_portuguese_interface,
        descrId = R.string.whats_new_thanks_to_users_contribute_weblate,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat"
      )
    )
  )
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

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewWhatsNewView() {
  SimpleXTheme {
    WhatsNewView(
      viaSettings = true,
      close = {}
    )
  }
}
