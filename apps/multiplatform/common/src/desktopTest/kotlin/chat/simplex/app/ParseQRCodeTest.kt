package chat.simplex.app

import chat.simplex.common.views.newchat.QRCodeType
import chat.simplex.common.views.newchat.isSecurityCode
import chat.simplex.common.views.newchat.parseQRCode
import kotlin.test.Test
import kotlin.test.assertFalse
import kotlin.test.assertTrue

// Coverage for the parseQRCode ordering that does NOT require the core native (FFI) library:
// migration file links and desktop xrcp addresses short-circuit before the markdown / server
// parsers run. The connection-link, server-address and unknown fixtures call into core
// (chat_parse_markdown / chat_parse_server) and so belong in the Android instrumented suite,
// where the native library is loaded (see impl plan C1.1).
class ParseQRCodeTest {
  @Test
  fun migrationLink_simplexFileScheme() {
    assertTrue(parseQRCode("simplex:/file#/?abc") is QRCodeType.MigrationLink)
  }

  @Test
  fun migrationLink_httpsFileForm() {
    assertTrue(parseQRCode("https://simplex.chat/file#/?abc") is QRCodeType.MigrationLink)
  }

  @Test
  fun desktopAddress_xrcpScheme() {
    assertTrue(parseQRCode("xrcp:/ca@host:5226#/?v=1") is QRCodeType.DesktopAddress)
  }

  @Test
  fun trimsWhitespaceBeforeClassifying() {
    assertTrue(parseQRCode("   xrcp:/ca@host:5226#/?v=1   ") is QRCodeType.DesktopAddress)
  }

  @Test
  fun migrationCheckedBeforeDesktop() {
    // A file link must classify as migration even though neither scheme overlaps.
    assertTrue(parseQRCode("simplex:/file#x") is QRCodeType.MigrationLink)
  }

  // isSecurityCode is FFI-free, so it is testable here (unlike the parseQRCode branch that
  // reaches it, which runs after the FFI parsers and so belongs in the instrumented suite).
  @Test
  fun securityCode_realShape_decimalDigitsWithSpaces() {
    // The exact output of `verificationCode (sha256Hash "abcd")` from the core test fixture
    // tests/ChatTests/Direct.hs — decimal digits in groups of 5 separated by spaces. This is
    // literally what VerifyCodeView QR-encodes, so it must classify as a security code.
    val code = "61889 38426 63934 09576 96390 79389 84124 85253 63658 69469 70853 37788 95900 68296 20156 25"
    assertTrue(isSecurityCode(code))
  }

  @Test
  fun securityCode_hexLetters_isNotRecognised() {
    // `show . os2ip` only emits decimal digits, so a hex blob must NOT be taken for a code.
    assertFalse(isSecurityCode("a3f5".repeat(16)))
  }

  @Test
  fun securityCode_ungroupedLongNumber_isNotRecognised() {
    // A bare long number (no space grouping) is not the verificationCode shape — must not match.
    assertFalse(isSecurityCode("6188938426639340957696390793898412485253"))
  }

  @Test
  fun securityCode_shortNumber_isNotRecognised() {
    assertFalse(isSecurityCode("12345 67890"))      // grouped, but only 10 digits total
  }

  @Test
  fun securityCode_nonDigits_isNotRecognised() {
    assertFalse(isSecurityCode("this is definitely not a security code, it has letters"))
  }
}
