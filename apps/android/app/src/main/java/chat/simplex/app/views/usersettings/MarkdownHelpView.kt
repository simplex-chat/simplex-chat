package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.*
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.Format
import chat.simplex.app.model.FormatColor
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.generalGetString

@Composable
fun MarkdownHelpView() {
  Column {
    Text(
      generalGetString(R.string.how_to_use_markdown),
      style = MaterialTheme.typography.h1,
    )
    Text(
      generalGetString(R.string.you_can_use_markdown_to_format_messages__prompt),
      Modifier.padding(vertical = 16.dp)
    )
    val bold = generalGetString(R.string.bold)
    val italic = generalGetString(R.string.italic)
    val strikethrough = generalGetString(R.string.strikethrough)
    val equation = generalGetString(R.string.a_plus_b)
    val colored = generalGetString(R.string.colored)
    val secret = generalGetString(R.string.secret)

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


@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewMarkdownHelpView() {
  SimpleXTheme {
    MarkdownHelpView()
  }
}
