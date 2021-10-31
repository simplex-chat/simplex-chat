package chat.simplex.app

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp

@Composable
fun MessageView(message: String) {
    Box(Modifier.padding(4.dp)) {
        Surface(color = Color.LightGray, modifier = Modifier.clip(shape = RoundedCornerShape(8.dp))) {
            Box(Modifier.padding(8.dp)) {
                Text(text = message)
            }
        }
    }
}

@Preview(showBackground = true,
        widthDp = 200,
        heightDp = 50,
        backgroundColor = android.graphics.Color.BLACK.toLong())
@Composable
fun DefaultMessageView() {
    // A surface container using the 'background' color from the theme
    Surface(color = Color.White, modifier = Modifier.padding(16.dp)) {
        MessageView(message = "Hello world!")
    }
}