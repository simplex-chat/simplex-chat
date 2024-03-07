package chat.simplex.common.views.database

import SectionItemView
import SectionTextFooter
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.common.ui.theme.SimplexGreen
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource

@Composable
actual fun SavePassphraseSetting(
  useKeychain: Boolean,
  initialRandomDBPassphrase: Boolean,
  storedKey: Boolean,
  minHeight: Dp,
  enabled: Boolean,
  onCheckedChange: (Boolean) -> Unit,
) {
  SectionItemView(minHeight = minHeight) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        if (storedKey) painterResource(MR.images.ic_vpn_key_filled) else painterResource(MR.images.ic_vpn_key_off_filled),
        stringResource(MR.strings.save_passphrase_in_keychain),
        tint = if (storedKey) SimplexGreen else MaterialTheme.colors.secondary
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      Text(
        stringResource(MR.strings.save_passphrase_in_keychain),
        Modifier.padding(end = 24.dp),
        color = Color.Unspecified
      )
      Spacer(Modifier.fillMaxWidth().weight(1f))
      DefaultSwitch(
        checked = useKeychain,
        onCheckedChange = onCheckedChange,
        enabled = enabled
      )
    }
  }
}

@Composable
actual fun DatabaseEncryptionFooter(
  useKeychain: MutableState<Boolean>,
  chatDbEncrypted: Boolean?,
  storedKey: MutableState<Boolean>,
  initialRandomDBPassphrase: MutableState<Boolean>,
  migration: Boolean,
) {
  if (chatDbEncrypted == false) {
    SectionTextFooter(generalGetString(MR.strings.database_is_not_encrypted))
  } else if (useKeychain.value) {
    if (storedKey.value) {
      SectionTextFooter(generalGetString(MR.strings.keychain_is_storing_securely))
      if (initialRandomDBPassphrase.value && !migration) {
        SectionTextFooter(generalGetString(MR.strings.encrypted_with_random_passphrase))
      } else {
        SectionTextFooter(annotatedStringResource(MR.strings.impossible_to_recover_passphrase))
      }
    } else {
      SectionTextFooter(generalGetString(MR.strings.keychain_allows_to_receive_ntfs))
    }
  } else {
    SectionTextFooter(generalGetString(MR.strings.you_have_to_enter_passphrase_every_time))
    SectionTextFooter(annotatedStringResource(MR.strings.impossible_to_recover_passphrase))
  }
}

actual fun encryptDatabaseSavedAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.encrypt_database_question),
    text = generalGetString(MR.strings.database_will_be_encrypted_and_passphrase_stored) + "\n" + storeSecurelySaved(),
    confirmText = generalGetString(MR.strings.encrypt_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

actual fun changeDatabaseKeySavedAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.change_database_passphrase_question),
    text = generalGetString(MR.strings.database_encryption_will_be_updated) + "\n" + storeSecurelySaved(),
    confirmText = generalGetString(MR.strings.update_database),
    onConfirm = onConfirm,
    destructive = false,
  )
}

actual fun removePassphraseAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.remove_passphrase_from_keychain),
    text = generalGetString(MR.strings.notifications_will_be_hidden) + "\n" + storeSecurelyDanger(),
    confirmText = generalGetString(MR.strings.remove_passphrase),
    onConfirm = onConfirm,
    destructive = true,
  )
}
