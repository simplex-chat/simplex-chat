package chat.simplex.common.views.usersettings

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.Format
import chat.simplex.common.model.FormatColor
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.SimpleXTheme
import com.icerockdev.library.MR

@Composable
fun MarkdownHelpView() {
  Column(
    Modifier
      .fillMaxWidth()
  ) {
    Text(stringResource(R.string.you_can_use_markdown_to_format_messages__prompt))
    Spacer(Modifier.height(DEFAULT_PADDING))
    val bold = stringResource(R.string.bold_font)
    val italic = stringResource(R.string.italic_font)
    val strikethrough = stringResource(R.string.strikethrough)
    val equation = stringResource(R.string.a_plus_b)
    val colored = stringResource(R.string.colored_font)
    val secret = stringResource(R.string.secret_text)

    MdFormat("*$bold*", bold, Format.Bold())
    MdFormat("_${italic}_", italic, Format.Italic())
    MdFormat("~$strikethrough~", strikethrough, Format.StrikeThrough())
    MdFormat("`$equation`", equation, Format.Snippet())
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
    Row {
      MdSyntax("#$secret#")
      SelectionContainer {
        Text(buildAnnotatedString {
          withStyle(Format.Secret().style) { append(secret) }
        })
      }
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
