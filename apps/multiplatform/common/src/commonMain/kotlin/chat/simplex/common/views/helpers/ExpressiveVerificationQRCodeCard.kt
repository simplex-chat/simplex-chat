package chat.simplex.common.views.helpers

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.CheckCircle
import androidx.compose.material.icons.filled.Lock
import androidx.compose.material.icons.filled.QrCode
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.lerp
import androidx.compose.ui.graphics.vector.rememberVectorPainter
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.platform.HapticPattern
import chat.simplex.common.platform.RichHaptics
import chat.simplex.res.MR
import chat.simplex.res.*

private val SecurityGreen = Color(0xFF2E7D32)
private val SecurityGreenLight = Color(0xFFE8F5E9)
private val SecurityGreenDark = Color(0xFF1B5E20)

@Composable
fun ExpressiveVerificationQRCodeCard(
  verificationUri: String,
  safetyNumberShort: String? = null,
  isVerified: Boolean = false,
  onScanOtherClick: (() -> Unit)? = null,
  onConfirmVerification: (() -> Unit)? = null,
  onClearVerification: (() -> Unit)? = null,
  size: Dp = 230.dp
) {
  val clipboard = LocalClipboardManager.current
  val isLight = MaterialTheme.colors.isLight
  val cardBg = lerp(
    MaterialTheme.colors.surface,
    if (isVerified) SecurityGreen else MaterialTheme.colors.primary,
    if (isLight) 0.05f else 0.08f
  )
  val badgeBg = if (isLight) SecurityGreenLight else SecurityGreenDark.copy(alpha = 0.35f)
  val badgeContentColor = if (isLight) SecurityGreenDark else Color(0xFF81C784)

  Surface(
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = 4.dp, vertical = 6.dp),
    shape = RoundedCornerShape(32.dp),
    color = cardBg,
    elevation = 0.dp
  ) {
    Column(
      modifier = Modifier
        .fillMaxWidth()
        .padding(18.dp),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.spacedBy(14.dp)
    ) {
      // 1. Expressive Security Badge
      Row(
        modifier = Modifier
          .clip(RoundedCornerShape(percent = 50))
          .background(badgeBg)
          .padding(horizontal = 14.dp, vertical = 6.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        Icon(
          painter = rememberVectorPainter(Icons.Default.Lock),
          contentDescription = null,
          tint = badgeContentColor,
          modifier = Modifier.size(16.dp)
        )
        Text(
          text = if (isVerified) "KEYS VERIFIED" else "END-TO-END ENCRYPTION",
          fontSize = 11.sp,
          fontWeight = FontWeight.Bold,
          letterSpacing = 0.8.sp,
          color = badgeContentColor
        )
      }

      // 2. QR Code Box with dynamic security border
      Box(
        modifier = Modifier
          .clip(RoundedCornerShape(32.dp))
          .border(
            width = 2.dp,
            color = if (isVerified) SecurityGreen.copy(alpha = 0.5f) else MaterialTheme.colors.onSurface.copy(alpha = 0.08f),
            shape = RoundedCornerShape(32.dp)
          )
          .background(Color.White)
          .padding(6.dp),
        contentAlignment = Alignment.Center
      ) {
        Material3ExpressiveQRCode(
          content = verificationUri,
          size = size,
          primaryColor = Color(0xFF191C1E),
          containerColor = Color.White
        )
      }

      // 3. Short safety number snippet with tap-to-copy
      if (safetyNumberShort != null) {
        Column(
          horizontalAlignment = Alignment.CenterHorizontally,
          modifier = Modifier
            .clip(RoundedCornerShape(12.dp))
            .clickable {
              RichHaptics.perform(HapticPattern.CLICK)
              clipboard.setText(AnnotatedString(verificationUri))
            }
            .padding(horizontal = 8.dp, vertical = 4.dp)
        ) {
          Text(
            text = "Session fingerprint (tap to copy)",
            fontSize = 11.sp,
            color = MaterialTheme.colors.onSurface.copy(alpha = 0.55f)
          )
          Spacer(Modifier.height(4.dp))
          Text(
            text = safetyNumberShort,
            fontSize = 14.sp,
            fontWeight = FontWeight.SemiBold,
            fontFamily = FontFamily.Monospace,
            letterSpacing = 1.2.sp,
            textAlign = TextAlign.Center,
            color = MaterialTheme.colors.onSurface
          )
        }
      }

      // 4. Action buttons
      Column(
        modifier = Modifier.fillMaxWidth(),
        verticalArrangement = Arrangement.spacedBy(8.dp)
      ) {
        if (!isVerified && onConfirmVerification != null) {
          Button(
            onClick = {
              RichHaptics.perform(HapticPattern.SUCCESS)
              onConfirmVerification()
            },
            modifier = Modifier
              .fillMaxWidth()
              .height(48.dp),
            shape = RoundedCornerShape(percent = 50),
            colors = ButtonDefaults.buttonColors(
              backgroundColor = SecurityGreen,
              contentColor = Color.White
            ),
            elevation = null
          ) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.CheckCircle),
              contentDescription = null,
              modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text(
              text = "Verify Keys",
              fontWeight = FontWeight.SemiBold,
              fontSize = 15.sp
            )
          }
        }

        if (onScanOtherClick != null) {
          OutlinedButton(
            onClick = {
              RichHaptics.perform(HapticPattern.CLICK)
              onScanOtherClick()
            },
            modifier = Modifier
              .fillMaxWidth()
              .height(48.dp),
            shape = RoundedCornerShape(percent = 50),
            colors = ButtonDefaults.outlinedButtonColors(
              backgroundColor = Color.Transparent,
              contentColor = MaterialTheme.colors.primary
            ),
            border = ButtonDefaults.outlinedBorder.copy(
              brush = androidx.compose.ui.graphics.SolidColor(MaterialTheme.colors.onSurface.copy(alpha = 0.15f))
            )
          ) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.QrCode),
              contentDescription = null,
              modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text(
              text = "Scan Contact's QR",
              fontWeight = FontWeight.Medium,
              fontSize = 14.sp
            )
          }
        }

        if (isVerified && onClearVerification != null) {
          OutlinedButton(
            onClick = {
              RichHaptics.perform(HapticPattern.TOGGLE_OFF)
              onClearVerification()
            },
            modifier = Modifier
              .fillMaxWidth()
              .height(46.dp),
            shape = RoundedCornerShape(percent = 50),
            colors = ButtonDefaults.outlinedButtonColors(
              backgroundColor = Color.Transparent,
              contentColor = MaterialTheme.colors.error
            ),
            border = ButtonDefaults.outlinedBorder.copy(
              brush = androidx.compose.ui.graphics.SolidColor(MaterialTheme.colors.error.copy(alpha = 0.4f))
            )
          ) {
            Icon(
              painter = rememberVectorPainter(Icons.Default.Lock),
              contentDescription = null,
              tint = MaterialTheme.colors.error,
              modifier = Modifier.size(18.dp)
            )
            Spacer(Modifier.width(8.dp))
            Text(
              text = "Clear Verification",
              fontWeight = FontWeight.Medium,
              fontSize = 14.sp,
              color = MaterialTheme.colors.error
            )
          }
        }
      }
    }
  }
}
