package chat.simplex.app.ui.harness

import android.content.Context
import android.content.SharedPreferences
import chat.simplex.common.model.AppPreferences
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.getPreferenceFilePath
import java.io.File

object PrefStores {
  private val stores = listOf(AppPreferences.SHARED_PREFS_ID, "chat.simplex.app.THEMES")
  private val themesYaml get() = File(getPreferenceFilePath("themes.yaml"))
  private var captured: Map<String, Map<String, *>> = emptyMap()
  private var yamlCapture: ByteArray? = null

  fun capture() {
    captured = stores.associateWith { prefs(it).all.toMap() }
    yamlCapture = themesYaml.takeIf { it.exists() }?.readBytes()
  }

  fun restore() {
    for ((name, values) in captured) {
      val e = prefs(name).edit().clear()
      values.forEach { (k, v) -> e.put(k, v) }
      check(e.commit()) { "could not restore preferences $name" }
    }
    val yaml = yamlCapture
    if (yaml == null) themesYaml.delete() else themesYaml.apply { parentFile?.mkdirs() }.writeBytes(yaml)
    UiTestApp.onMain { CommonPrefs.reload() }
  }

  private fun prefs(name: String): SharedPreferences = androidAppContext.getSharedPreferences(name, Context.MODE_PRIVATE)

  @Suppress("UNCHECKED_CAST")
  private fun SharedPreferences.Editor.put(k: String, v: Any?) {
    when (v) {
      null -> remove(k)
      is String -> putString(k, v)
      is Int -> putInt(k, v)
      is Long -> putLong(k, v)
      is Float -> putFloat(k, v)
      is Boolean -> putBoolean(k, v)
      is Set<*> -> putStringSet(k, v as Set<String>)
      else -> error("unsupported preference type ${v.javaClass} for $k")
    }
  }
}
