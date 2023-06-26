package chat.simplex.common.helpers

import android.app.Activity
import android.content.res.Configuration
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.defaultLocale
import java.util.*

//import android.app.LocaleManager

fun Activity.applyAppLocale(pref: SharedPreference<String?>) {
  //  if (Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU) {
  val lang = pref.get()
  if (lang == null || lang == Locale.getDefault().language) return
  applyLocale(Locale.forLanguageTag(lang))
  //  }
}

private fun Activity.applyLocale(locale: Locale) {
  Locale.setDefault(locale)
  val appConf = Configuration(androidAppContext.resources.configuration).apply { setLocale(locale) }
  val activityConf = Configuration(resources.configuration).apply { setLocale(locale) }
  @Suppress("DEPRECATION")
  androidAppContext.resources.updateConfiguration(appConf, resources.displayMetrics)
  @Suppress("DEPRECATION")
  resources.updateConfiguration(activityConf, resources.displayMetrics)
}

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