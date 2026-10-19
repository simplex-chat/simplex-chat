package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.ui.Ui
import org.junit.Test

class ChatProfilesTest : UiTest() {
  @Test fun profilesAddProfile() = uiTest {
    val alice = app("alice")
    val profiles = alice.userPicker().profiles()
    waitFor(Ui.Profiles.Add)
    poll(what = "the profiles list with `alice` and without `work`") { seesText("alice") && !seesText("work") }
    profiles.add("work")
    poll(what = "the profiles list with `alice` and `work`") { exists(Ui.Profiles.Add) && seesText("alice") && seesText("work") }
  }
}
