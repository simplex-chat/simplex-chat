package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.labelled
import chat.simplex.app.ui.harness.leaveProfileEditorExpectingSaveAlert
import chat.simplex.app.ui.harness.profileEditorSave
import chat.simplex.app.ui.harness.setFullNameViaApi
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

class ProfileEditTest : UiTest() {
  @Test fun editProfileBioAndFullName() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.profile()
    poll(what = "the profile editor with `Bio:` and without `Full name:`") {
      exists(Ui.Profile.Bio) && exists(Ui.Profile.Save) && seesText("Bio:") && !exists(Ui.Profile.FullName) && !seesText("Full name:")
    }
    check(!isEnabled(Ui.Profile.Save)) { "`Save and notify contacts` is enabled before any change" }
    typeInto(Ui.Profile.Bio, "loves cats")
    profileEditorSave()
    bob.eventually("contact alice updated bio: loves cats", CONNECT_TIMEOUT_MS)
    poll(what = "the profile editor to close after saving") { !exists(Ui.Profile.Save) }

    setFullNameViaApi("Alice Smith")
    bob.eventually("contact alice updated full name: Alice Smith", CONNECT_TIMEOUT_MS)
    alice.profile()
    poll(what = "`Full name:` with `Alice Smith` and the saved bio") {
      seesText("Full name:") && editableText(Ui.Profile.FullName) == "Alice Smith" && editableText(Ui.Profile.Bio) == "loves cats"
    }
    replaceText(Ui.Profile.FullName, "Alice Jones")
    profileEditorSave()
    bob.eventually("contact alice updated full name: Alice Jones", CONNECT_TIMEOUT_MS)
    poll(what = "the profile editor to close and the full name saved") {
      !exists(Ui.Profile.Save) && chatModel.currentUser.value?.profile?.fullName == "Alice Jones"
    }
  }

  @Test fun editProfileUnsavedChangesAlertExitAndSave() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.profile()
    replaceText(Ui.Profile.Name, "carol")
    leaveProfileEditorExpectingSaveAlert()
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the profile editor closed with alice unchanged") { !exists(Ui.Profile.Name) && chatModel.currentUser.value?.displayName == "alice" }

    alice.profile()
    poll(what = "the profile editor showing `alice` again") { editableText(Ui.Profile.Name) == "alice" }
    replaceText(Ui.Profile.Name, "alice2")
    leaveProfileEditorExpectingSaveAlert()
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    bob.eventually("contact alice changed to alice2", CONNECT_TIMEOUT_MS)
    poll(what = "the profile editor closed with alice2 as the current user") { !exists(Ui.Profile.Name) && chatModel.currentUser.value?.displayName == "alice2" }
  }

  @Test fun editProfileInvalidNameDisablesSave() = uiTest {
    val alice = app("alice")
    alice.userPicker().profiles().apply { add("work"); close() }
    alice.userPicker().switchTo("alice")
    alice.profile()
    poll(what = "the profile editor with `alice`") { editableText(Ui.Profile.Name) == "alice" && exists(Ui.Profile.Save) }
    check(!isEnabled(Ui.Profile.Save) && !exists(NAME_INVALID) && !exists(BIO_INVALID)) { "Save is enabled, or an error icon is shown, before any change" }

    replaceText(Ui.Profile.Name, "alice2")
    poll(what = "Save enabled for a new valid name") { isEnabled(Ui.Profile.Save) }
    replaceText(Ui.Profile.Name, "alice")
    poll(what = "Save disabled again with the name changed back") { !isEnabled(Ui.Profile.Save) }
    replaceText(Ui.Profile.Name, "   ")
    poll(what = "Save disabled for a blank name") { editableText(Ui.Profile.Name) == "   " && !isEnabled(Ui.Profile.Save) }
    replaceText(Ui.Profile.Name, "?alice2")
    poll(what = "the invalid-name icon and Save disabled for `?alice2`") { exists(NAME_INVALID) && !isEnabled(Ui.Profile.Save) }
    click(NAME_INVALID)
    poll(what = "the `Invalid name!` alert offering `alice2`") { seesText("Invalid name!") && seesText("Correct name to alice2?") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the name corrected to `alice2` with Save enabled") {
      editableText(Ui.Profile.Name) == "alice2" && !exists(NAME_INVALID) && !seesText("Invalid name!") && isEnabled(Ui.Profile.Save)
    }

    typeInto(Ui.Profile.Bio, "x".repeat(MAX_BIO + 1))
    poll(what = "the bio error icon and Save disabled for a ${MAX_BIO + 1}-character bio") { exists(BIO_INVALID) && !isEnabled(Ui.Profile.Save) }
    click(BIO_INVALID)
    poll(what = "the `Bio too large` alert") { seesText("Bio too large") }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the alert closed") { !seesText("Bio too large") }
    replaceText(Ui.Profile.Bio, "x".repeat(MAX_BIO))
    poll(what = "no bio error icon and Save enabled for a $MAX_BIO-character bio") { !exists(BIO_INVALID) && isEnabled(Ui.Profile.Save) }

    replaceText(Ui.Profile.Name, "work")
    poll(what = "Save enabled for the other profile's name") { !exists(NAME_INVALID) && isEnabled(Ui.Profile.Save) }
    profileEditorSave()
    poll(what = "the `Duplicate display name!` alert") {
      seesText("Duplicate display name!") && seesText("You already have a chat profile with the same display name. Please choose another name.")
    }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the editor kept open with alice unchanged") {
      !seesText("Duplicate display name!") && exists(Ui.Profile.Save) &&
        chatModel.currentUser.value?.let { it.displayName == "alice" && it.shortDescr == null } == true
    }
  }

  @Test fun editProfileDescription() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    alice.profile()
    poll(what = "`Add description` in the profile editor") { labelled(Ui.Profile.Description, "Add description") && !exists(Ui.Profile.DescriptionText) }
    click(Ui.Profile.Description)
    poll(what = "the description editor in place of the profile fields") { exists(Ui.Profile.DescriptionText) && !exists(Ui.Profile.Save) }
    typeInto(Ui.Profile.DescriptionText, DESCRIPTION)
    hideKeyboard()
    goBack()
    poll(what = "the profile fields back with `Edit description`") {
      labelled(Ui.Profile.Description, "Edit description") && exists(Ui.Profile.Save) && !exists(Ui.Profile.DescriptionText)
    }
    profileEditorSave()
    bob.eventually("contact alice updated description: $DESCRIPTION", CONNECT_TIMEOUT_MS)
    poll(what = "the profile editor closed with the description saved") {
      !exists(Ui.Profile.Save) && chatModel.currentUser.value?.profileDescription == DESCRIPTION
    }
    alice.profile()
    poll(what = "`Edit description` after reopening") { labelled(Ui.Profile.Description, "Edit description") }
    click(Ui.Profile.Description)
    poll(what = "the saved description in the editor") { editableText(Ui.Profile.DescriptionText) == DESCRIPTION }
  }

  private companion object {
    val NAME_INVALID = Ui.Profile.Name.part("invalid")
    val BIO_INVALID = Ui.Profile.Bio.part("invalid")
    const val MAX_BIO = 160
    const val DESCRIPTION = "I like long walks"
  }
}
