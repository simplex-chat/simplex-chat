package chat.simplex.app.views.helpers

import android.content.Context
import android.content.res.Resources
import android.graphics.*
import android.graphics.Typeface
import android.text.Spanned
import android.text.SpannedString
import android.text.style.*
import android.view.ViewTreeObserver
import androidx.annotation.StringRes
import androidx.compose.runtime.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.*
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.*
import androidx.compose.ui.text.style.BaselineShift
import androidx.compose.ui.text.style.TextDecoration
import androidx.compose.ui.unit.*
import androidx.core.content.FileProvider
import androidx.core.text.HtmlCompat
import chat.simplex.app.BuildConfig
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.CIFile
import kotlinx.coroutines.*
import java.io.File

fun withApi(action: suspend CoroutineScope.() -> Unit): Job =
  GlobalScope.launch { withContext(Dispatchers.Main, action) }

enum class KeyboardState {
  Opened, Closed
}

@Composable
fun getKeyboardState(): State<KeyboardState> {
  val keyboardState = remember { mutableStateOf(KeyboardState.Closed) }
  val view = LocalView.current
  DisposableEffect(view) {
    val onGlobalListener = ViewTreeObserver.OnGlobalLayoutListener {
      val rect = Rect()
      view.getWindowVisibleDisplayFrame(rect)
      val screenHeight = view.rootView.height
      val keypadHeight = screenHeight - rect.bottom
      keyboardState.value = if (keypadHeight > screenHeight * 0.15) {
        KeyboardState.Opened
      } else {
        KeyboardState.Closed
      }
    }
    view.viewTreeObserver.addOnGlobalLayoutListener(onGlobalListener)

    onDispose {
      view.viewTreeObserver.removeOnGlobalLayoutListener(onGlobalListener)
    }
  }

  return keyboardState
}
// Resource to annotated string from
// https://stackoverflow.com/questions/68549248/android-jetpack-compose-how-to-show-styled-text-from-string-resources
fun generalGetString(id: Int): String {
  return SimplexApp.context.getString(id)
}

@Composable
@ReadOnlyComposable
private fun resources(): Resources {
  LocalConfiguration.current
  return LocalContext.current.resources
}

fun Spanned.toHtmlWithoutParagraphs(): String {
  return HtmlCompat.toHtml(this, HtmlCompat.TO_HTML_PARAGRAPH_LINES_CONSECUTIVE)
    .substringAfter("<p dir=\"ltr\">").substringBeforeLast("</p>")
}

fun Resources.getText(@StringRes id: Int, vararg args: Any): CharSequence {
  val escapedArgs = args.map {
    if (it is Spanned) it.toHtmlWithoutParagraphs() else it
  }.toTypedArray()
  val resource = SpannedString(getText(id))
  val htmlResource = resource.toHtmlWithoutParagraphs()
  val formattedHtml = String.format(htmlResource, *escapedArgs)
  return HtmlCompat.fromHtml(formattedHtml, HtmlCompat.FROM_HTML_MODE_LEGACY)
}

@Composable
fun annotatedStringResource(@StringRes id: Int): AnnotatedString {
  val resources = resources()
  val density = LocalDensity.current
  return remember(id) {
    val text = resources.getText(id)
    spannableStringToAnnotatedString(text, density)
  }
}

private fun spannableStringToAnnotatedString(
  text: CharSequence,
  density: Density,
): AnnotatedString {
  return if (text is Spanned) {
    with(density) {
      buildAnnotatedString {
        append((text.toString()))
        text.getSpans(0, text.length, Any::class.java).forEach {
          val start = text.getSpanStart(it)
          val end = text.getSpanEnd(it)
          when (it) {
            is StyleSpan -> when (it.style) {
              Typeface.NORMAL -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Normal,
                  fontStyle = FontStyle.Normal,
                ),
                start,
                end
              )
              Typeface.BOLD -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Bold,
                  fontStyle = FontStyle.Normal
                ),
                start,
                end
              )
              Typeface.ITALIC -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Normal,
                  fontStyle = FontStyle.Italic
                ),
                start,
                end
              )
              Typeface.BOLD_ITALIC -> addStyle(
                SpanStyle(
                  fontWeight = FontWeight.Bold,
                  fontStyle = FontStyle.Italic
                ),
                start,
                end
              )
            }
            is TypefaceSpan -> addStyle(
              SpanStyle(
                fontFamily = when (it.family) {
                  FontFamily.SansSerif.name -> FontFamily.SansSerif
                  FontFamily.Serif.name -> FontFamily.Serif
                  FontFamily.Monospace.name -> FontFamily.Monospace
                  FontFamily.Cursive.name -> FontFamily.Cursive
                  else -> FontFamily.Default
                }
              ),
              start,
              end
            )
            is AbsoluteSizeSpan -> addStyle(
              SpanStyle(fontSize = if (it.dip) it.size.dp.toSp() else it.size.toSp()),
              start,
              end
            )
            is RelativeSizeSpan -> addStyle(
              SpanStyle(fontSize = it.sizeChange.em),
              start,
              end
            )
            is StrikethroughSpan -> addStyle(
              SpanStyle(textDecoration = TextDecoration.LineThrough),
              start,
              end
            )
            is UnderlineSpan -> addStyle(
              SpanStyle(textDecoration = TextDecoration.Underline),
              start,
              end
            )
            is SuperscriptSpan -> addStyle(
              SpanStyle(baselineShift = BaselineShift.Superscript),
              start,
              end
            )
            is SubscriptSpan -> addStyle(
              SpanStyle(baselineShift = BaselineShift.Subscript),
              start,
              end
            )
            is ForegroundColorSpan -> addStyle(
              SpanStyle(color = Color(it.foregroundColor)),
              start,
              end
            )
            else -> addStyle(SpanStyle(color = Color.White), start, end)
          }
        }
      }
    }
  } else {
    AnnotatedString(text.toString())
  }
}

fun getFilesDirectory(context: Context): String {
  return context.filesDir.toString()
}

fun getAppFilesDirectory(context: Context): String {
  return getFilesDirectory(context) + "/app_files"
}

fun getStoredFilePath(context: Context, file: CIFile?): String? {
  return if (file?.filePath != null && file.stored) {
    val filePath = getAppFilesDirectory(context) + "/" + file.filePath
    if (File(filePath).exists()) filePath else null
  } else {
    null
  }
}

// https://developer.android.com/training/data-storage/shared/documents-files#bitmap
fun getStoredImage(context: Context, file: CIFile?): Bitmap? {
  val filePath = getStoredFilePath(context, file)
  return if (filePath != null) {
    try {
      val uri = FileProvider.getUriForFile(context, "${BuildConfig.APPLICATION_ID}.provider", File(filePath))
      val parcelFileDescriptor = context.contentResolver.openFileDescriptor(uri, "r")
      val fileDescriptor = parcelFileDescriptor?.fileDescriptor
      val image = BitmapFactory.decodeFileDescriptor(fileDescriptor)
      parcelFileDescriptor?.close()
      image
    } catch (e: Exception) {
      null
    }
  } else {
    null
  }
}
