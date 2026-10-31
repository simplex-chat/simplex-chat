package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.helpAndSupport
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.platform
import chat.simplex.common.ui.Ui
import org.junit.Test

private val HELP_ROWS = listOf(
  "How to use it", "What's new", "About SimpleX Chat",
  "Follow SimpleX Network News", "Send questions and ideas", "Send us email", "Star on GitHub",
)
private const val WELCOME = "Welcome alice!"
private const val HELP_TEXT = "Thank you for installing SimpleX Chat!"
private const val ABOUT_TEXT = "Private and secure messaging."
private const val WHY_BUILT = "Why SimpleX is built."

class HelpTest : UiTest() {
  @Test fun help_layoutHowToUseAndAbout() = uiTest {
    val alice = app("alice")
    helpAndSupport(alice)
    check(HELP_ROWS.all { seesText(it) }) { "Help & support rows missing: ${HELP_ROWS.filterNot { seesText(it) }}" }
    check(seesText("Rate the app") == appPlatform.isAndroid) { "Rate the app shown: ${seesText("Rate the app")} on ${appPlatform.name}" }
    check(seesText("Contribute") == !platform.androidIsPlayStoreBuild) { "Contribute shown: ${seesText("Contribute")}" }
    check(!seesText(WELCOME) && !seesText(ABOUT_TEXT)) { "How to use it or About is shown before it was opened" }
    click(Ui.Help.HowToUse)
    poll(what = "How to use it with alice's welcome and the help text") { seesText(WELCOME) && seesText(HELP_TEXT) && !exists(Ui.Help.HowToUse) }
    escape()
    poll(what = "Help & support again") { exists(Ui.Help.About) && !seesText(WELCOME) }
    click(Ui.Help.About)
    poll(what = "About SimpleX Chat without the onboarding buttons") {
      seesText(ABOUT_TEXT) && !exists(Ui.Help.About) && !exists(Ui.Onboarding.WhyBuilt) && !seesText(WHY_BUILT)
    }
    escape()
    poll(what = "Help & support after About") { exists(Ui.Help.About) && !seesText(ABOUT_TEXT) }
  }
}
