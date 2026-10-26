package chat.simplex.app.ui.harness

import androidx.compose.runtime.MutableState
import chat.simplex.common.AppLock
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.platform.VideoPlayerHolder
import chat.simplex.common.views.badges.BadgeStore
import chat.simplex.common.views.chat.chatViewScrollState
import kotlinx.coroutines.flow.MutableStateFlow
import java.lang.reflect.Field
import java.lang.reflect.Modifier
import java.util.IdentityHashMap

object ModelSnapshot {
  private val restores = mutableListOf<() -> Unit>()
  private val excludedTypes = setOf(ChatController::class.java, AppPreferences::class.java)

  fun capture() {
    restores.clear()
    val seen = IdentityHashMap<Any, Unit>()
    listOf<Any>(ChatModel, AppLock, ConnectProgressManager, ChannelRelaysModel, BadgeModel, BadgeStore, VideoPlayerHolder).forEach { walk(it, seen) }
  }

  fun restore(onMain: (() -> Unit) -> Unit) = onMain { restores.forEach { it() } }

  private fun walk(obj: Any, seen: IdentityHashMap<Any, Unit>) {
    if (seen.put(obj, Unit) != null) return
    val cls = obj.javaClass
    val isObject = runCatching { cls.getDeclaredField("INSTANCE").apply { isAccessible = true }.get(null) === obj }.getOrDefault(false)
    for (f in cls.declaredFields) {
      val static = Modifier.isStatic(f.modifiers)
      if (static != isObject || f.isSynthetic || f.name == "INSTANCE" || f.name.startsWith("$")) continue
      if (f.type in excludedTypes) continue
      f.isAccessible = true
      val target = if (static) null else obj
      val value = f.get(target)
      if (!Modifier.isFinal(f.modifiers)) restores += { f.set(target, value) }
      val content = if (value is Lazy<*>) value.value else value
      if (content != null) captureValue(content)
      if (content != null && Modifier.isFinal(f.modifiers) && isAppObject(content)) walk(content, seen)
    }
  }

  @Suppress("UNCHECKED_CAST")
  private fun captureValue(value: Any) {
    when {
      value is MutableStateFlow<*> -> { val v = value.value; restores += { (value as MutableStateFlow<Any?>).value = v } }
      value is MutableState<*> -> { val v = value.value; restores += { (value as MutableState<Any?>).value = v } }
      value is MutableList<*> -> { val c = value.toList(); restores += { (value as MutableList<Any?>).apply { clear(); addAll(c) } } }
      value is MutableCollection<*> -> { val c = value.toList(); restores += { (value as MutableCollection<Any?>).apply { clear(); addAll(c) } } }
      value is MutableMap<*, *> -> { val c = value.toMap(); restores += { (value as MutableMap<Any?, Any?>).apply { clear(); putAll(c) } } }
    }
  }

  private fun isAppObject(v: Any): Boolean =
    v.javaClass.name.startsWith("chat.simplex.") && !v.javaClass.isEnum && v.javaClass !in excludedTypes
}

object CommonUiState {
  fun clear(onMain: (() -> Unit) -> Unit) {
    ModelSnapshot.restore(onMain)
    onMain {
      CIFile.cachedRemoteFileRequests.clear()
      clearImageCaches()
      WallpaperType.cachedImages.clear()
      CommonFields.chatListState.field.set(null, 0 to 0)
      CommonFields.reportsListState.field.set(null, null)
      chatViewScrollState.value = false
      CommonFields.badgeLinkStep.field.set(null, null)
      AppLock.clearAuthState()
      CommonFields.selectedOperatorIds.field.set(null, emptySet<Long>())
    }
    clearTransient(onMain)
  }

  fun clearTransient(onMain: (() -> Unit) -> Unit) = onMain {
    ModalManager.closeAllModalsEverywhere()
    listOf(ModalManager.start, ModalManager.center, ModalManager.end, ModalManager.fullscreen).distinct().forEach { m ->
      for (ref in CommonFields.passcodeViews) {
        @Suppress("UNCHECKED_CAST")
        (ref.field.get(m) as MutableStateFlow<Any?>).value = null
      }
    }
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
  }
}

object CommonPrefs {
  fun reload() {
    CommonFields.themeOverridesStore.field.set(null, null)
    val stateField = CommonFields.prefState.field
    for (f in AppPreferences::class.java.declaredFields) {
      if (!SharedPreference::class.java.isAssignableFrom(f.type)) continue
      f.isAccessible = true
      val pref = f.get(ChatController.appPrefs) as SharedPreference<*>
      @Suppress("UNCHECKED_CAST")
      (stateField.get(pref) as MutableState<Any?>).value = pref.get()
    }
    val appPrefs = ChatController.appPrefs
    chat.simplex.common.ui.theme.ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)
    java.util.Locale.setDefault(java.util.Locale.ENGLISH)
    val chatModel = ChatModel
    chatModel.notificationPreviewMode.value =
      runCatching { NotificationPreviewMode.valueOf(appPrefs.notificationPreviewMode.get()!!) }.getOrDefault(NotificationPreviewMode.default)
    chatModel.showAuthScreen.value = appPrefs.performLA.get()
    chatModel.showChatPreviews.value = appPrefs.privacyShowChatPreviews.get()
    chatModel.simplexLinkMode.value = appPrefs.simplexLinkMode.get()
  }
}

class FieldRef(private val cls: String, private val name: String) {
  val field: Field get() = Class.forName(cls, false, FieldRef::class.java.classLoader).getDeclaredField(name).apply { isAccessible = true }
  override fun toString() = "$cls.$name"
}

object CommonFields {
  val chatListState = FieldRef("chat.simplex.common.views.chatlist.ChatListViewKt", "lazyListState")
  val reportsListState = FieldRef("chat.simplex.common.views.chat.ChatViewKt", "reportsListState")
  val badgeLinkStep = FieldRef("chat.simplex.common.views.badges.BadgesRedeemCodeViewKt", "badgeLinkStep")
  val selectedOperatorIds = FieldRef("chat.simplex.common.views.onboarding.OnboardingSharedState", "selectedOperatorIds")
  val passcodeViews = listOf("passcodeView", "onTimePasscodeView").map { FieldRef("chat.simplex.common.views.helpers.ModalManager", it) }
  val themeOverridesStore = FieldRef("chat.simplex.common.model.AppPreferences", "themeOverridesStore")
  val prefState = FieldRef("chat.simplex.common.model.SharedPreference", "_state")
  val whatsNewLastVersion = FieldRef("chat.simplex.common.views.onboarding.WhatsNewViewKt", "lastVersion")
  val all: List<FieldRef> get() = listOf(chatListState, reportsListState, badgeLinkStep, selectedOperatorIds, themeOverridesStore, prefState, whatsNewLastVersion) + passcodeViews
}
