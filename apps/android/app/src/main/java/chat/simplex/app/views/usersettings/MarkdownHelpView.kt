package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.Format
import chat.simplex.app.model.FormatColor
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.CloseSheetBar

@Composable
fun MarkdownHelpView(nav: NavController) {
  MarkdownHelpLayout(nav::popBackStack)
}

@Composable
fun MarkdownHelpLayout(back: () -> Unit) {
  Surface(
    Modifier
      .background(MaterialTheme.colors.background)
      .fillMaxSize()
  ) {
    Column {
      CloseSheetBar(back)
      Column(Modifier.padding(horizontal = 16.dp)) {
        Text(
          "How to use markdown",
          style = MaterialTheme.typography.h1,
        )
        Text(
          "You can use markdown to format messages:",
          Modifier.padding(vertical = 16.dp)
        )
        MdFormat("*bold*", "bold text", Format.Bold())
        MdFormat("_italic_", "italic text", Format.Italic())
        MdFormat("~strike~", "strikethrough text", Format.StrikeThrough())
        MdFormat("`code`", "a = b + c", Format.Snippet())
        Row {
          MdSyntax("!1 colored!")
          Text(buildAnnotatedString {
            withStyle(Format.Colored(FormatColor.red).style) { append("red text") }
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
          MdSyntax("#secret")
          SelectionContainer {
            Text(buildAnnotatedString {
              withStyle(Format.Secret().style) { append("secret text") }
            })
          }
        }
      }
    }
  }
}

@Composable
fun MdSyntax(markdown: String) {
  Text(markdown, Modifier
    .width(100.dp)
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
    MarkdownHelpLayout(back = {})
  }
}
