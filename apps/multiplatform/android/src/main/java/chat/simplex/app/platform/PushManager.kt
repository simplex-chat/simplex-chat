package chat.simplex.app.platform

import SectionItemView
import android.content.Context
import android.content.pm.PackageManager
import android.os.Build
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CornerSize
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.getUserServers
import chat.simplex.common.platform.Log
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.showAddServerDialog
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import org.unifiedpush.android.connector.UnifiedPush
import androidx.core.net.toUri

/**
 * Object with functions to interact with push services
 */
object PushManager {
  private const val TAG = "PushManager"
  private val VAPID_REGEX = Regex("^[A-Za-z0-9_-]{87}(=+)?$")

  /**
   * Check if a NTF server is setup and ask user's push distributor
   *
   * Show an alert if NTF server isn't define,
   * If a single distrib is available, use it
   * If many, ask distributor to use with a dialog
   * Else alert about missing service
   */
  suspend fun initUnifiedPush(context: Context, scope: CoroutineScope, onSuccess: () -> Unit) {
    val rh = chatModel.remoteHostId()
    val userServers = getUserServers(rh) ?: listOf()
    val userNtfServer = userServers.userNtfServer()
      ?: run {
      Log.d(TAG, "User doesn't have any NTF server")
      showMissingNTFDialog(scope, rh, userServers)
      // After coming back from the server view, users will have to click on "Instant" again
      return
    }
    val vapid = userNtfServer.vapid()
      ?: run {
        Log.d(TAG, "User NTF Server doesn't have any VAPID FP")
        showNTFNoVAPIDDialog(scope, rh, userServers)
        // After coming back from the server view, users will have to click on "Instant" again
        return
      }
    val distributors = UnifiedPush.getDistributors(context)
    when (distributors.size) {
      0 -> {
        Log.d(TAG, "No distributor found")
        showMissingPushServiceDialog()
      }
      1 -> {
        Log.d(TAG, "Found only one distributor installed")
        UnifiedPush.saveDistributor(context, distributors.first())
        register(context, vapid)
        onSuccess()
      }
      else -> {
        Log.d(TAG, "Found many distributors installed")
        showSelectPushServiceIntroDialog {
          showSelectPushServiceDialog(context, distributors) {
            UnifiedPush.saveDistributor(context, it)
            register(context, vapid)
            onSuccess()
          }
        }
      }
    }
  }

  /**
   * Register to UnifiedPush distributor if any is already used
   *
   * To run when the app starts; call [chat.simplex.app.PushService.onUnregistered]
   * if the distributor is uninstalled
   */
  suspend fun initStart(context: Context) {
    Log.d(TAG, "Init UnifiedPush during app startup")
    //TODO: limit to once a day to reduce registrations to ntf server ?
    UnifiedPush.getAckDistributor(context)?.let {
      val vapid = getUserServers(chatModel.remoteHostId())
        ?.userNtfServer()
        ?.vapid()
        // Unregister if the user deleted the ntf server or change it with one without vapid
        ?: return UnifiedPush.unregister(context)
      register(context, vapid)
    }
  }

  private fun register(context: Context, vapid: String) {
    UnifiedPush.register(context, vapid = vapid)
  }

  /**
   * Get user NTF server
   */
  private fun List<UserOperatorServers>.userNtfServer(): UserServer? {
    return this.firstNotNullOfOrNull {
      it.ntfServers.firstOrNull { s -> s.enabled }
    }
  }

  /**
   * Get vapid fingerprint from the URI
   */
  private fun UserServer.vapid(): String ? = server
    .toUri()
    .getQueryParameter("vapid")
    ?.takeIf { VAPID_REGEX.matches(it) }

  /**
   * Show a dialog to inform about missing NTF server
   */
  private fun showMissingNTFDialog(scope: CoroutineScope, rh: Long?, userServers: List<UserOperatorServers>) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
          Icon(
            painterResource(MR.images.ic_warning),
            contentDescription = null, // The icon doesn't add any meaning and must not be transcripted with screen readers
          )
          Text(
            stringResource(MR.strings.icon_descr_instant_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Text(stringResource(MR.strings.warning_push_needs_ntf_server))
      },
      // Go to user's servers settings
      confirmButton = {
        TextButton(onClick = {
          AlertManager.shared.hideAlert()
          showAddServerDialog(scope, rh, userServers)
        }) { Text(stringResource(MR.strings.smp_servers_add)) }
      },
      // Ignore
      dismissButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(android.R.string.cancel)) }
      },
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    )
  }

  /**
   * Show a dialog to inform about missing VAPID with the NTF server
   */
  private fun showNTFNoVAPIDDialog(scope: CoroutineScope, rh: Long?, userServers: List<UserOperatorServers>) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
          Icon(
            painterResource(MR.images.ic_warning),
            contentDescription = null, // The icon doesn't add any meaning and must not be transcripted with screen readers
          )
          Text(
            stringResource(MR.strings.icon_descr_instant_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Text(stringResource(MR.strings.warning_push_needs_ntf_server_with_vapid))
      },
      // Go to user's servers settings
      confirmButton = {
        TextButton(onClick = {
          AlertManager.shared.hideAlert()
          showAddServerDialog(scope, rh, userServers)
        }) { Text(stringResource(MR.strings.smp_servers_add)) }
      },
      // Ignore
      dismissButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(android.R.string.cancel)) }
      },
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    )
  }
  /**
   * Dialog to add a server, manually or with a QR code
   */
  private fun showAddServerDialog(scope: CoroutineScope, rh: Long?, userServers: List<UserOperatorServers>) {
    val userServersState = mutableStateOf(userServers)
    val serverErrors = mutableStateOf(listOf<UserServersError>())
    showAddServerDialog(scope, userServersState, serverErrors, rh)
  }

  /**
   * Show a dialog to inform about missing push service
   */
  private fun showMissingPushServiceDialog() = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
          Icon(
            painterResource(MR.images.ic_warning),
            contentDescription = null, // The icon doesn't add any meaning and must not be transcripted with screen readers
          )
          Text(
            stringResource(MR.strings.icon_descr_instant_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Text(
          buildAnnotatedString {
            append(stringResource(MR.strings.warning_push_needs_push_service))
            withLink(LinkAnnotation.Url(url = "https://unifiedpush.org")) {
              withStyle(style = SpanStyle(color = MaterialTheme.colors.primary)) {
                append("https://unifiedpush.org")
              }
            }
          }
        )
      },
      confirmButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(android.R.string.ok)) }
      },
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    )
  }

  /**
   * Show an intro to explain why they need to select a push service
   */
  private fun showSelectPushServiceIntroDialog(onConfirm: () -> Unit) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
          Icon(
            painterResource(MR.images.ic_notifications),
            contentDescription = null, // The icon doesn't add any meaning and must not be transcripted with screen readers
          )
          Text(
            stringResource(MR.strings.icon_descr_instant_notifications),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Text(stringResource(MR.strings.select_push_service_intro))
      },
      confirmButton = {
        TextButton(
          onClick = {
          AlertManager.shared.hideAlert()
          onConfirm()
          }
        ) { Text(stringResource(MR.strings.select_push_service)) }
      },
      dismissButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(android.R.string.cancel)) }
      },
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    )
  }


  /**
   * Show a dialog to select push service
   *
   * @param distributors List of installed distributors' packageName
   * @param onSelected run when a distributor is selected, its param is the selected distrib packageName
   */
  private fun showSelectPushServiceDialog(context: Context, distributors: List<String>, onSelected: (String) -> Unit) = AlertManager.shared.showAlert {
    AlertDialog(
      onDismissRequest = AlertManager.shared::hideAlert,
      title = {
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
          Icon(
            painterResource(MR.images.ic_notifications),
            contentDescription = null, // The icon doesn't add any meaning and must not be transcripted with screen readers
          )
          Text(
            stringResource(MR.strings.select_push_service),
            fontWeight = FontWeight.Bold
          )
        }
      },
      text = {
        Column {
          distributors.map {
            if (it == context.packageName) it to "Play Services"
            else it to context.getApplicationName(it)
          }.forEach {
            SectionItemView({
              AlertManager.shared.hideAlert()
              onSelected(it.first)
            }) {
              Text(it.second)
            }
          }
        }
      },
      confirmButton = {
        TextButton(onClick = AlertManager.shared::hideAlert) { Text(stringResource(android.R.string.cancel)) }
      },
      shape = RoundedCornerShape(corner = CornerSize(25.dp))
    )
  }

  private fun Context.getApplicationName(applicationId: String): String {
    return try {
      val ai = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
        packageManager.getApplicationInfo(
          applicationId,
          PackageManager.ApplicationInfoFlags.of(
            PackageManager.GET_META_DATA.toLong()
          )
        )
      } else {
        packageManager.getApplicationInfo(applicationId, 0)
      }
      packageManager.getApplicationLabel(ai)
    } catch (e: PackageManager.NameNotFoundException) {
      applicationId
    } as String
  }

}