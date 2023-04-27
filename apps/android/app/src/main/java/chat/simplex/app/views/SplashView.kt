package chat.simplex.app.views

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier

@Composable
fun SplashView() {
  Surface(
    Modifier
      .fillMaxSize(),
    color = MaterialTheme.colors.background
  ) {
//    Image(
//      painter = painterResource(R.drawable.logo),
//      contentDescription = "Simplex Icon",
//      modifier = Modifier
//        .height(230.dp)
//        .align(Alignment.Center)
//    )
  }
}
