package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.input.VisualTransformation

@Composable
actual fun platformText(text: AnnotatedString, style: TextStyle): PlatformText = PlatformText(text, style)

@Composable
actual fun platformTextFieldStyle(text: String, style: TextStyle, visualTransformation: VisualTransformation): PlatformTextFieldStyle =
  PlatformTextFieldStyle(style, visualTransformation)
