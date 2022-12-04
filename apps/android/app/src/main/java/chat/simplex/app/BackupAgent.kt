package chat.simplex.app

import android.app.backup.BackupAgentHelper
import android.app.backup.FullBackupDataOutput
import android.content.Context
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.model.AppPreferences.Companion.SHARED_PREFS_PRIVACY_FULL_BACKUP

class BackupAgent: BackupAgentHelper() {
  override fun onFullBackup(data: FullBackupDataOutput?) {
    if (applicationContext
        .getSharedPreferences(AppPreferences.SHARED_PREFS_ID, Context.MODE_PRIVATE)
        .getBoolean(SHARED_PREFS_PRIVACY_FULL_BACKUP, true)
    ) {
      super.onFullBackup(data)
    }
  }
}
