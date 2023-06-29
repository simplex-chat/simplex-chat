package chat.simplex.app.views.localauth

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.ui.graphics.painter.Painter
import androidx.compose.ui.res.painterResource
import chat.simplex.app.R
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.*

@Composable
fun PasscodeEntry(
  password: MutableState<String>,
  vertical: Boolean,
) {
  Column(horizontalAlignment = Alignment.CenterHorizontally) {
    PasscodeView(password)
    BoxWithConstraints {
      if (vertical) {
        VerticalPasswordGrid(password)
      } else {
        HorizontalPasswordGrid(password)
      }
    }
  }
}

@Composable
fun PasscodeView(password: MutableState<String>) {
  var showPasscode by rememberSaveable { mutableStateOf(false) }
  Text(
    if (password.value.isEmpty()) " " else remember(password.value, showPasscode) { splitPassword(showPasscode, password.value) },
    Modifier.padding(vertical = 10.dp).clickable { showPasscode = !showPasscode },
    style = MaterialTheme.typography.body1
  )
}

@Composable
private fun BoxWithConstraintsScope.VerticalPasswordGrid(password: MutableState<String>) {
  val s = minOf(maxWidth, maxHeight) / 4 - 1.dp
  Column(Modifier.width(IntrinsicSize.Min)) {
    DigitsRow(s, 1, 2, 3, password)
    Divider()
    DigitsRow(s, 4, 5, 6, password)
    Divider()
    DigitsRow(s, 7, 8, 9, password)
    Divider()
    Row(Modifier.requiredHeight(s)) {
      PasswordEdit(s, painterResource(R.drawable.ic_close)) {
        password.value = ""
      }
      VerticalDivider()
      PasswordDigit(s, 0, password)
      VerticalDivider()
      PasswordEdit(s, painterResource(R.drawable.ic_backspace)) {
        password.value = password.value.dropLast(1)
      }
    }
  }
}

@Composable
private fun BoxWithConstraintsScope.HorizontalPasswordGrid(password: MutableState<String>) {
  val s = minOf(maxWidth, maxHeight) / 3.5f - 1.dp
  Column(Modifier.width(IntrinsicSize.Min)) {
    Row(Modifier.height(IntrinsicSize.Min)) {
      DigitsRow(s, 1, 2, 3, password);
      VerticalDivider()
      PasswordEdit(s, painterResource(R.drawable.ic_close)) {
        password.value = ""
      }
    }
    Divider()
    Row(Modifier.height(IntrinsicSize.Min)) {
      DigitsRow(s, 4, 5, 6, password)
      VerticalDivider()
      PasswordDigit(s, 0, password)
    }
    Divider()
    Row(Modifier.height(IntrinsicSize.Min)) {
      DigitsRow(s, 7, 8, 9, password)
      VerticalDivider()
      PasswordEdit(s, painterResource(R.drawable.ic_backspace)) {
        password.value = password.value.dropLast(1)
      }
    }
  }
}

private fun splitPassword(showPassword: Boolean, password: String): String {
  val n = if (password.length < 8) 8 else 4
  return password.mapIndexed { index, c -> (if (showPassword) c.toString() else "●") + (if ((index + 1) % n == 0) " " else "") }.joinToString("")
}

@Composable
private fun DigitsRow(size: Dp, d1: Int, d2: Int, d3: Int, password: MutableState<String>) {
  Row(Modifier.height(size)) {
    PasswordDigit(size, d1, password)
    VerticalDivider()
    PasswordDigit(size, d2, password)
    VerticalDivider()
    PasswordDigit(size, d3, password)
  }
}

@Composable
private fun PasswordDigit(size: Dp, d: Int, password: MutableState<String>) {
  val s = d.toString()
  return PasswordButton(size, action = {
    if (password.value.length < 16) {
      password.value += s
    }
  }) {
    Text(
      s,
      style = TextStyle(
        fontWeight = FontWeight.Normal,
        fontSize = 30.sp,
        letterSpacing = (-0.5).sp
      ),
      color = MaterialTheme.colors.secondary
    )
  }
}

@Composable
private fun PasswordEdit(size: Dp, image: Painter, action: () -> Unit) {
  PasswordButton(size, action) {
    Icon(image, null, tint = MaterialTheme.colors.secondary)
  }
}

@Composable
private fun PasswordButton(size: Dp, action: () -> Unit, content: @Composable BoxScope.() -> Unit) {
  return Box(
    Modifier.size(size)
      .background(MaterialTheme.colors.background, RoundedCornerShape(50))
      .clickable { action() },
    contentAlignment = Alignment.Center
  ) {
    content()
  }
}

@Composable
fun VerticalDivider(
  modifier: Modifier = Modifier,
  color: Color = MaterialTheme.colors.onSurface.copy(alpha = DividerAlpha),
  thickness: Dp = 1.dp,
  startIndent: Dp = 0.dp
) {
  val indentMod = if (startIndent.value != 0f) {
    Modifier.padding(top = startIndent)
  } else {
    Modifier
  }
  val targetThickness = if (thickness == Dp.Hairline) {
    (1f / LocalDensity.current.density).dp
  } else {
    thickness
  }
  Box(
    modifier.then(indentMod)
      .fillMaxHeight()
      .width(targetThickness)
      .background(color = color)
  )
}

private const val DividerAlpha = 0.12f
