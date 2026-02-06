package chat.simplex.app

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.RestrictionsManager
import android.os.Bundle
import chat.simplex.common.model.ServerAddress
import chat.simplex.common.model.UserServer
import chat.simplex.common.platform.Log
import kotlinx.coroutines.*

data class MdmServerConfig(
    val smpServers: List<UserServer>,
    val xftpServers: List<UserServer>,
    val lockConfiguration: Boolean
)

object MdmConfigManager {
    private const val TAG = "MdmConfigManager"

    // Restriction keys matching app_restrictions.xml
    private const val KEY_SMP_SERVERS = "mdm_smp_servers"
    private const val KEY_XFTP_SERVERS = "mdm_xftp_servers"
    private const val KEY_LOCK_CONFIG = "mdm_lock_server_config"

    private var configChangeListener: (suspend () -> Unit)? = null
    private var receiver: BroadcastReceiver? = null

    fun getMdmConfig(context: Context): MdmServerConfig? {
        val restrictionsManager = context.getSystemService(Context.RESTRICTIONS_SERVICE)
            as? RestrictionsManager ?: return null

        val restrictions = restrictionsManager.applicationRestrictions
        // Only parse if at least one of our MDM keys is explicitly set by admin.
        // On managed devices, applicationRestrictions can return a non-empty Bundle
        // with default values from app_restrictions.xml even without admin configuration.
        if (!restrictions.containsKey(KEY_SMP_SERVERS) &&
            !restrictions.containsKey(KEY_XFTP_SERVERS) &&
            !restrictions.containsKey(KEY_LOCK_CONFIG)) return null

        return parseMdmConfig(restrictions)
    }

    fun hasMdmConfig(context: Context): Boolean {
        val restrictionsManager = context.getSystemService(Context.RESTRICTIONS_SERVICE)
            as? RestrictionsManager ?: return false
        val restrictions = restrictionsManager.applicationRestrictions
        // Check if any of our MDM keys are present
        return restrictions.containsKey(KEY_SMP_SERVERS) ||
               restrictions.containsKey(KEY_XFTP_SERVERS) ||
               restrictions.containsKey(KEY_LOCK_CONFIG)
    }

    fun isConfigLocked(context: Context): Boolean {
        return getMdmConfig(context)?.lockConfiguration ?: false
    }

    private fun parseMdmConfig(restrictions: Bundle): MdmServerConfig {
        val lockConfig = restrictions.getBoolean(KEY_LOCK_CONFIG, false)

        val smpServersString = restrictions.getString(KEY_SMP_SERVERS, "")
        val xftpServersString = restrictions.getString(KEY_XFTP_SERVERS, "")

        val smpServers = parseServerList(smpServersString)
        val xftpServers = parseServerList(xftpServersString)

        Log.d(TAG, "Parsed MDM config: ${smpServers.size} SMP servers, ${xftpServers.size} XFTP servers, locked=$lockConfig")

        return MdmServerConfig(
            smpServers = smpServers,
            xftpServers = xftpServers,
            lockConfiguration = lockConfig
        )
    }

    private fun parseServerList(serversString: String): List<UserServer> {
        if (serversString.isBlank()) return emptyList()

        return serversString
            .split(",")
            .map { it.trim() }
            .filter { it.isNotBlank() }
            .mapNotNull { address ->
                // Validate server address format
                val parsed = ServerAddress.parseServerAddress(address)
                if (parsed == null) {
                    Log.w(TAG, "Invalid MDM server address: $address")
                    return@mapNotNull null
                }

                UserServer(
                    remoteHostId = null,
                    serverId = null,
                    server = address,
                    preset = false,  // MDM servers are not preset
                    tested = null,
                    enabled = true,
                    deleted = false
                )
            }
    }

    fun registerConfigChangeListener(context: Context, listener: suspend () -> Unit) {
        configChangeListener = listener

        if (receiver == null) {
            receiver = object : BroadcastReceiver() {
                override fun onReceive(context: Context, intent: Intent) {
                    if (intent.action == Intent.ACTION_APPLICATION_RESTRICTIONS_CHANGED) {
                        Log.d(TAG, "MDM configuration changed")
                        val pendingResult = goAsync()
                        CoroutineScope(SupervisorJob()).launch(Dispatchers.Default) {
                            try {
                                configChangeListener?.invoke()
                            } catch (e: Exception) {
                                Log.e(TAG, "Failed to apply MDM config change: ${e.message}")
                            } finally {
                                pendingResult.finish()
                            }
                        }
                    }
                }
            }

            context.registerReceiver(
                receiver,
                IntentFilter(Intent.ACTION_APPLICATION_RESTRICTIONS_CHANGED),
                Context.RECEIVER_NOT_EXPORTED
            )
        }
    }

    fun unregisterConfigChangeListener(context: Context) {
        receiver?.let {
            try {
                context.unregisterReceiver(it)
            } catch (e: IllegalArgumentException) {
                // Receiver not registered, ignore
            }
            receiver = null
        }
        configChangeListener = null
    }
}
