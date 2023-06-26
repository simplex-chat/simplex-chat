package chat.simplex.common.helpers

import android.net.Uri
import android.os.Build
import chat.simplex.common.model.NotificationsMode
import java.net.URI

val NotificationsMode.requiresIgnoringBatterySinceSdk: Int get() = when(this) {
  NotificationsMode.OFF -> Int.MAX_VALUE
  NotificationsMode.PERIODIC -> Build.VERSION_CODES.M
  NotificationsMode.SERVICE -> Build.VERSION_CODES.S
  /*INSTANT -> Int.MAX_VALUE - for Firebase notifications */
}

val NotificationsMode.requiresIgnoringBattery
  get() = requiresIgnoringBatterySinceSdk <= Build.VERSION.SDK_INT

lateinit var APPLICATION_ID: String

fun Uri.toURI(): URI = URI(toString())

fun URI.toUri(): Uri = Uri.parse(toString())