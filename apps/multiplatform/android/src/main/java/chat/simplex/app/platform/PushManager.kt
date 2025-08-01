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
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.networkAndServers.showAddServerDialog
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import org.unifiedpush.android.connector.UnifiedPush

/**
 * Object with functions to interact with push services
 */
object PushManager {
  private const val TAG = "PushManager"

  /**
   * Show an alert if NTF server isn't define,
   * If a single distrib is available, use it
   * If many, ask distributor to use with a dialog
   * Else alert about missing service
   */
  suspend fun initUnifiedPush(context: Context, scope: CoroutineScope, onSuccess: () -> Unit) {
    val distributors = UnifiedPush.getDistributors(context)
    when (distributors.size) {
      0 -> {
        Log.d(TAG, "No distributor found")
        showMissingPushServiceDialog()
      }
      1 -> {
        Log.d(TAG, "Found only one distributor installed")
        UnifiedPush.saveDistributor(context, distributors.first())
        register(context)
        onSuccess()
      }
      else -> {
        Log.d(TAG, "Found many distributors installed")
        showSelectPushServiceIntroDialog {
          showSelectPushServiceDialog(context, distributors) {
            UnifiedPush.saveDistributor(context, it)
            register(context)
            onSuccess()
          }
        }
      }
    }
  }

  private fun register(context: Context) {
    // TODO: add VAPID
    UnifiedPush.register(context)
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
