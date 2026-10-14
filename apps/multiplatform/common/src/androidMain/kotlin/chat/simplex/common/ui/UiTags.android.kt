package chat.simplex.common.ui

import android.content.Context
import android.content.pm.ApplicationInfo

fun initUiTags(context: Context) {
  uiTestHooksEnabled = context.applicationInfo.flags and ApplicationInfo.FLAG_DEBUGGABLE != 0
}
