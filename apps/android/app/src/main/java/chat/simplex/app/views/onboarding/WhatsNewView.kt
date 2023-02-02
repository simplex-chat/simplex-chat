package chat.simplex.app.views.onboarding

import android.content.res.Configuration
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*

@Composable
fun WhatsNewView(viaSettings: Boolean = false, close: () -> Unit) {
  val currentVersion = remember { mutableStateOf(versionDescriptions.lastIndex) }

  @Composable
  fun featureDescription(icon: ImageVector, titleId: Int, descrId: Int, link: String?) {
    @Composable
    fun linkButton(link: String) {
      val uriHandler = LocalUriHandler.current
      Icon(
        Icons.Outlined.OpenInNew, stringResource(titleId), tint = MaterialTheme.colors.primary,
        modifier = Modifier
          .clickable { uriHandler.openUri(link) }
      )
    }

    Column(
      horizontalAlignment = Alignment.Start
    ) {
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Icon(icon, stringResource(titleId), tint = HighOrLowlight)
        Text(
          generalGetString(titleId),
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.h3,
          fontWeight = FontWeight.Medium
        )
        if (link != null) {
          linkButton(link)
        }
      }
      Text(generalGetString(descrId))
    }
  }

  @Composable
  fun pagination() {
    Row(
      Modifier
        .padding(bottom = 16.dp)
    ) {
      if (currentVersion.value > 0) {
        val prev = currentVersion.value - 1
        Surface(shape = RoundedCornerShape(20.dp)) {
          Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier
              .clickable { currentVersion.value = prev }
              .padding(8.dp)
          ) {
            Icon(Icons.Outlined.ArrowBackIosNew, "previous", tint = MaterialTheme.colors.primary)
            Text(versionDescriptions[prev].version, color = MaterialTheme.colors.primary)
          }
        }
      }
      Spacer(Modifier.fillMaxWidth().weight(1f))
      if (currentVersion.value < versionDescriptions.lastIndex) {
        val next = currentVersion.value + 1
        Surface(shape = RoundedCornerShape(20.dp)) {
          Row(
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier
              .clickable { currentVersion.value = next }
              .padding(8.dp)
          ) {
            Text(versionDescriptions[next].version, color = MaterialTheme.colors.primary)
            Icon(Icons.Outlined.ArrowForwardIos, "next", tint = MaterialTheme.colors.primary)
          }
        }
      }
    }
  }

  val v = versionDescriptions[currentVersion.value]

  ModalView(close = close) {
    Column(
      Modifier
        .fillMaxWidth()
        .padding(horizontal = DEFAULT_PADDING),
      horizontalAlignment = Alignment.Start,
      verticalArrangement = Arrangement.spacedBy(16.dp)
    ) {
      Text(
        String.format(generalGetString(R.string.new_in_version), v.version),
        Modifier
          .fillMaxWidth()
          .padding(DEFAULT_PADDING),
        textAlign = TextAlign.Center,
        overflow = TextOverflow.Ellipsis,
        style = MaterialTheme.typography.h1,
        fontWeight = FontWeight.Normal,
        color = HighOrLowlight
      )

      v.features.forEach { feature ->
        featureDescription(feature.icon, feature.titleId, feature.descrId, feature.link)
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
  val icon: ImageVector,
  val titleId: Int,
  val descrId: Int,
  val link: String? = null
)

private data class VersionDescription(
  val version: String,
  val features: List<FeatureDescription>
)

private val versionDescriptions: List<VersionDescription> = listOf(
  VersionDescription(
    version = "v4.2",
    features = listOf(
      FeatureDescription(
        icon = Icons.Outlined.VerifiedUser,
        titleId = R.string.v4_2_security_assessment,
        descrId = R.string.v4_2_security_assessment_desc,
        link = "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"
      ),
      FeatureDescription(
        icon = Icons.Outlined.Group,
        titleId = R.string.v4_2_group_links,
        descrId = R.string.v4_2_group_links_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.Check,
        titleId = R.string.v4_2_auto_accept_contact_requests,
        descrId = R.string.v4_2_auto_accept_contact_requests_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.3",
    features = listOf(
      FeatureDescription(
        icon = Icons.Outlined.Mic,
        titleId = R.string.v4_3_voice_messages,
        descrId = R.string.v4_3_voice_messages_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.DeleteForever,
        titleId = R.string.v4_3_irreversible_message_deletion,
        descrId = R.string.v4_3_irreversible_message_deletion_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.WifiTethering,
        titleId = R.string.v4_3_improved_server_configuration,
        descrId = R.string.v4_3_improved_server_configuration_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.VisibilityOff,
        titleId = R.string.v4_3_improved_privacy_and_security,
        descrId = R.string.v4_3_improved_privacy_and_security_desc
      ),
    )
  ),
  VersionDescription(
    version = "v4.4",
    features = listOf(
      FeatureDescription(
        icon = Icons.Outlined.Timer,
        titleId = R.string.v4_4_disappearing_messages,
        descrId = R.string.v4_4_disappearing_messages_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.Pending,
        titleId = R.string.v4_4_live_messages,
        descrId = R.string.v4_4_live_messages_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.VerifiedUser,
        titleId = R.string.v4_4_verify_connection_security,
        descrId = R.string.v4_4_verify_connection_security_desc
      ),
      FeatureDescription(
        icon = Icons.Outlined.Translate,
        titleId = R.string.v4_4_french_interface,
        descrId = R.string.v4_4_french_interface_descr
      )
    )
  ),
  VersionDescription(
    version = "v4.5",
    features = listOf(
      FeatureDescription(
        icon = Icons.Outlined.ManageAccounts,
        titleId = R.string.v4_5_multiple_chat_profiles,
        descrId = R.string.v4_5_multiple_chat_profiles_descr
      ),
      FeatureDescription(
        icon = Icons.Outlined.EditNote,
        titleId = R.string.v4_5_message_draft,
        descrId = R.string.v4_5_message_draft_descr
      ),
      FeatureDescription(
        icon = Icons.Outlined.SafetyDivider,
        titleId = R.string.v4_5_transport_isolation,
        descrId = R.string.v4_5_transport_isolation_descr,
        link = "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation"
      ),
      FeatureDescription(
        icon = Icons.Outlined.Task,
        titleId = R.string.v4_5_private_filenames,
        descrId = R.string.v4_5_private_filenames_descr
      ),
      FeatureDescription(
        icon = Icons.Outlined.Battery2Bar,
        titleId = R.string.v4_5_reduced_battery_usage,
        descrId = R.string.v4_5_reduced_battery_usage_descr
      ),
      FeatureDescription(
        icon = Icons.Outlined.Translate,
        titleId = R.string.v4_5_italian_interface,
        descrId = R.string.v4_5_italian_interface_descr,
        link = "https://github.com/simplex-chat/simplex-chat/tree/stable#translate-the-apps"
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
