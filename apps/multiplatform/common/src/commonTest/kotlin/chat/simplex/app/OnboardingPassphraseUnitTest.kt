package chat.simplex.app

import chat.simplex.common.views.database.validKey
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class OnboardingPassphraseUnitTest {

  @Test
  fun asciiPassphraseIsValid() {
    assertTrue(validKey("abc"))
    assertTrue(validKey("Uitest-passphrase-7"))
    assertTrue(validKey("!~"))
  }

  @Test
  fun emptyPassphraseIsValidKey() {
    assertTrue(validKey(""))
  }

  @Test
  fun whitespaceMakesPassphraseInvalid() {
    assertFalse(validKey("a b"))
    assertFalse(validKey("a\tb"))
    assertFalse(validKey("ab\n"))
    assertFalse(validKey(" "))
  }

  @Test
  fun nonAsciiMakesPassphraseInvalid() {
    assertFalse(validKey("pässword"))
    assertFalse(validKey("пароль"))
    assertFalse(validKey("a\u007Fb"))
    assertFalse(validKey("a\u0001b"))
  }
}
