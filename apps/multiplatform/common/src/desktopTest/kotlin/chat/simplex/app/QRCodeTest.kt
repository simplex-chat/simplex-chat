package chat.simplex.app

import boofcv.alg.fiducial.qrcode.QrCode
import chat.simplex.common.views.newchat.qrCodeBitmap
import kotlin.test.Test
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class QRCodeTest {
  private val link = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D"

  @Test
  fun testQRCodeBitmapOfLinkWithinCapacity() {
    assertNotNull(qrCodeBitmap(link, errorLevel = QrCode.ErrorLevel.L))
    assertNotNull(qrCodeBitmap(link, errorLevel = QrCode.ErrorLevel.M))
  }

  @Test
  fun testNoQRCodeBitmapOfLinkAboveCapacity() {
    assertNull(qrCodeBitmap("$link&data=${"a".repeat(3000)}", errorLevel = QrCode.ErrorLevel.L))
    assertNull(qrCodeBitmap("$link&data=${"a".repeat(2400)}", errorLevel = QrCode.ErrorLevel.M))
  }
}
