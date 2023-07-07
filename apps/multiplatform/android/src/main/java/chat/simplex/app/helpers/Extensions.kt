package chat.simplex.app.helpers

import android.app.Activity
import android.content.res.Configuration
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.SharedPreference
import chat.simplex.app.platform.defaultLocale
import java.util.*

fun saveAppLocale(pref: SharedPreference<String?>, activity: Activity, languageCode: String? = null) {
  //  if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
  //    val localeManager = SimplexApp.context.getSystemService(LocaleManager::class.java)
  //    localeManager.applicationLocales = LocaleList(Locale.forLanguageTag(languageCode ?: return))
  //  } else {
  pref.set(languageCode)
  if (languageCode == null) {
    activity.applyLocale(defaultLocale)
  }
  activity.recreate()
  //  }
}

fun Activity.applyAppLocale(pref: SharedPreference<String?>) {
  //  if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
  val lang = pref.get()
  if (lang == null || lang == Locale.getDefault().language) return
  applyLocale(Locale.forLanguageTag(lang))
  //  }
}

private fun Activity.applyLocale(locale: Locale) {
  Locale.setDefault(locale)
  val appConf = Configuration(SimplexApp.context.resources.configuration).apply { setLocale(locale) }
  val activityConf = Configuration(resources.configuration).apply { setLocale(locale) }
  @Suppress("DEPRECATION")
  SimplexApp.context.resources.updateConfiguration(appConf, resources.displayMetrics)
  @Suppress("DEPRECATION")
  resources.updateConfiguration(activityConf, resources.displayMetrics)
}
