package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.foundation.clickable
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.input.pointer.PointerIcon
import androidx.compose.ui.input.pointer.pointerHoverIcon
import androidx.compose.ui.platform.LocalUriHandler
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.Format
import chat.simplex.common.model.FormatColor
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.helpers.openExternalLink
import chat.simplex.res.MR

@Composable
fun MarkdownHelpView() {
  Column(
    Modifier
      .fillMaxWidth()
  ) {
    Text(stringResource(MR.strings.you_can_use_markdown_to_format_messages__prompt))
    Spacer(Modifier.height(DEFAULT_PADDING))
    val bold = stringResource(MR.strings.bold_text)
    val italic = stringResource(MR.strings.italic_text)
    val strikethrough = stringResource(MR.strings.strikethrough_text)
    val equation = stringResource(MR.strings.a_plus_b)
    val colored = stringResource(MR.strings.colored_text)
    val secret = stringResource(MR.strings.secret_text)
    val small = stringResource(MR.strings.small_text)
    val link = stringResource(MR.strings.link_text)
    val uriHandler = LocalUriHandler.current

    MdFormat("*$bold*", bold, Format.Bold())
    MdFormat("_${italic}_", italic, Format.Italic())
    MdFormat("~$strikethrough~", strikethrough, Format.StrikeThrough())
    MdFormat("`$equation`", equation, Format.Snippet())
    MdFormat("!- $small!", small, Format.Small())
    Row {
      MdSyntax("!1 $colored!")
      Text(buildAnnotatedString {
        withStyle(Format.Colored(FormatColor.red).style) { append(colored) }
        append(" (")
        appendColor(this, "1", FormatColor.red, ", ")
        appendColor(this, "2", FormatColor.green, ", ")
        appendColor(this, "3", FormatColor.blue, ", ")
        appendColor(this, "4", FormatColor.yellow, ", ")
        appendColor(this, "5", FormatColor.cyan, ", ")
        appendColor(this, "6", FormatColor.magenta, ")")
      })
    }
    var secretRevealed by remember { mutableStateOf(false) }
    Row {
      MdSyntax("#$secret#")
      Text(
        buildAnnotatedString {
          if (secretRevealed) append(secret)
          else withStyle(Format.Secret().style) { append(secret) }
        },
        mdClickable { secretRevealed = !secretRevealed }
      )
    }
    Row {
      MdSyntax("[$link](https://simplex.chat)")
      Text(
        buildAnnotatedString {
          withStyle(Format.HyperLink(link, "https://simplex.chat").style) { append(link) }
        },
        mdClickable { uriHandler.openExternalLink("https://simplex.chat") }
      )
    }
    SectionBottomSpacer()
  }
}

@Composable
fun MdSyntax(markdown: String) {
  Text(markdown, Modifier
    .width(120.dp)
    .padding(bottom = 4.dp))
}

// hand cursor on hover, no click highlight - matches how secret text and links behave in chat
@Composable
fun mdClickable(onClick: () -> Unit): Modifier =
  Modifier
    .pointerHoverIcon(PointerIcon.Hand)
    .clickable(interactionSource = remember { MutableInteractionSource() }, indication = null, onClick = onClick)

@Composable
fun MdFormat(markdown: String, example: String, format: Format) {
  Row {
    MdSyntax(markdown)
    Text(buildAnnotatedString {
      withStyle(format.style) { append(example) }
    })
  }
}

@Composable
fun appendColor(b: AnnotatedString.Builder, s: String, c: FormatColor, after: String) {
  b.withStyle(Format.Colored(c).style) { append(s)}
  b.append(after)
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewMarkdownHelpView() {
  SimpleXTheme {
    MarkdownHelpView()
  }
}
