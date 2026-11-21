package chat.simplex.common.platform

import androidx.compose.foundation.text.InlineTextContent
import androidx.compose.material.LocalTextStyle
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.TextLayoutResult
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.TextUnit

class PlatformText(val text: AnnotatedString, val style: TextStyle)

@Composable
expect fun platformText(text: AnnotatedString, style: TextStyle): PlatformText

class PlatformTextFieldStyle(val style: TextStyle, val visualTransformation: VisualTransformation)

@Composable
expect fun platformTextFieldStyle(text: String, style: TextStyle, visualTransformation: VisualTransformation = VisualTransformation.None): PlatformTextFieldStyle

@Composable
fun Text(
  text: String,
  modifier: Modifier = Modifier,
  color: Color = Color.Unspecified,
  fontSize: TextUnit = TextUnit.Unspecified,
  fontStyle: FontStyle? = null,
  fontWeight: FontWeight? = null,
  fontFamily: FontFamily? = null,
  letterSpacing: TextUnit = TextUnit.Unspecified,
  textDecoration: TextDecoration? = null,
  textAlign: TextAlign? = null,
  lineHeight: TextUnit = TextUnit.Unspecified,
  overflow: TextOverflow = TextOverflow.Clip,
  softWrap: Boolean = true,
  maxLines: Int = Int.MAX_VALUE,
  minLines: Int = 1,
  onTextLayout: ((TextLayoutResult) -> Unit)? = null,
  style: TextStyle = LocalTextStyle.current
) {
  if (appPlatform.isAndroid) {
    androidx.compose.material.Text(
      text, modifier, color, fontSize, fontStyle, fontWeight, fontFamily, letterSpacing, textDecoration, textAlign, lineHeight,
      overflow, softWrap, maxLines, minLines, onTextLayout, style
    )
  } else {
    val merged = mergedStyle(style, fontSize, fontStyle, fontWeight, fontFamily, letterSpacing, textDecoration, textAlign, lineHeight)
    val laidOut = platformText(AnnotatedString(text), merged)
    if (laidOut.text.spanStyles.isEmpty()) {
      androidx.compose.material.Text(
        text, modifier, color,
        overflow = overflow, softWrap = softWrap, maxLines = maxLines, minLines = minLines, onTextLayout = onTextLayout, style = laidOut.style
      )
    } else {
      androidx.compose.material.Text(
        laidOut.text, modifier, color,
        overflow = overflow, softWrap = softWrap, maxLines = maxLines, minLines = minLines, onTextLayout = onTextLayout ?: {}, style = laidOut.style
      )
    }
  }
}

@Composable
fun Text(
  text: AnnotatedString,
  modifier: Modifier = Modifier,
  color: Color = Color.Unspecified,
  fontSize: TextUnit = TextUnit.Unspecified,
  fontStyle: FontStyle? = null,
  fontWeight: FontWeight? = null,
  fontFamily: FontFamily? = null,
  letterSpacing: TextUnit = TextUnit.Unspecified,
  textDecoration: TextDecoration? = null,
  textAlign: TextAlign? = null,
  lineHeight: TextUnit = TextUnit.Unspecified,
  overflow: TextOverflow = TextOverflow.Clip,
  softWrap: Boolean = true,
  maxLines: Int = Int.MAX_VALUE,
  minLines: Int = 1,
  inlineContent: Map<String, InlineTextContent> = mapOf(),
  onTextLayout: (TextLayoutResult) -> Unit = {},
  style: TextStyle = LocalTextStyle.current
) {
  if (appPlatform.isAndroid) {
    androidx.compose.material.Text(
      text, modifier, color, fontSize, fontStyle, fontWeight, fontFamily, letterSpacing, textDecoration, textAlign, lineHeight,
      overflow, softWrap, maxLines, minLines, inlineContent, onTextLayout, style
    )
  } else {
    val merged = mergedStyle(style, fontSize, fontStyle, fontWeight, fontFamily, letterSpacing, textDecoration, textAlign, lineHeight)
    val laidOut = platformText(text, merged)
    androidx.compose.material.Text(
      laidOut.text, modifier, color,
      overflow = overflow, softWrap = softWrap, maxLines = maxLines, minLines = minLines,
      inlineContent = inlineContent, onTextLayout = onTextLayout, style = laidOut.style
    )
  }
}

private fun mergedStyle(
  style: TextStyle,
  fontSize: TextUnit,
  fontStyle: FontStyle?,
  fontWeight: FontWeight?,
  fontFamily: FontFamily?,
  letterSpacing: TextUnit,
  textDecoration: TextDecoration?,
  textAlign: TextAlign?,
  lineHeight: TextUnit
): TextStyle =
  style.merge(
    fontSize = fontSize,
    fontWeight = fontWeight,
    textAlign = textAlign ?: TextAlign.Unspecified,
    lineHeight = lineHeight,
    fontFamily = fontFamily,
    textDecoration = textDecoration,
    fontStyle = fontStyle,
    letterSpacing = letterSpacing
  )
