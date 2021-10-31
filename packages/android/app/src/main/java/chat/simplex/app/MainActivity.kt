package chat.simplex.app

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Send
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment.Companion.Center
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch

class MainActivity : ComponentActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContent {
            var history by remember { mutableStateOf(mutableListOf("Session started:")) }
            var inputValue by remember { mutableStateOf("") }
            var inProgress by remember { mutableStateOf(false) }

            fun sendMessage() {
                CoroutineScope(Dispatchers.IO).launch {
                    inProgress = true
                    val str = Protocol.executeCommand(inputValue)
                    inputValue = ""
                    val newList = mutableListOf<String>()
                    newList.addAll(history)
                    newList.add(str)
                    Thread.sleep(1000); // emulate work
                    history = newList
                    inProgress = false
                }
            }

            MaterialTheme(colors = lightColors()) {
                Surface(color = Color.White) {
                    Column {
                        LazyColumn(Modifier.weight(1.0f).fillMaxWidth()) {
                            itemsIndexed(history) { index: Int, message: String ->
                                if (index == 0) {
                                    Spacer(modifier = Modifier.height(8.dp))
                                }
                                MessageView(message = message)
                            }
                        }
                        Row {
                            TextField(
                                modifier = Modifier.weight(1f),
                                value = inputValue,
                                onValueChange = { inputValue = it },
                                keyboardOptions = KeyboardOptions(imeAction = androidx.compose.ui.text.input.ImeAction.Send),
                                keyboardActions = KeyboardActions { sendMessage() },
                                singleLine = true
                            )
                            if (inProgress) {
                                Box(Modifier.height(56.dp).width(56.dp)) {
                                    CircularProgressIndicator(modifier = Modifier.align(Center))
                                }
                            }
                            else {
                                Button(
                                    modifier = Modifier.height(56.dp),
                                    onClick = { sendMessage() },
                                    enabled = inputValue.isNotBlank(),
                                ) {
                                    Icon(
                                        imageVector = Icons.Default.Send,
                                        contentDescription = "Send"
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}