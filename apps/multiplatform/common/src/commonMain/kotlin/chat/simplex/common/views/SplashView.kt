package chat.simplex.common.views

import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier

@Composable
fun SplashView() {
  Surface(
    Modifier
      .fillMaxSize(),
    color = MaterialTheme.colors.background,
    contentColor = LocalContentColor.current
  ) {
//    Image(
//      painter = painterResource(MR.images.logo),
//      contentDescription = "Simplex Icon",
//      modifier = Modifier
//        .height(230.dp)
//        .align(Alignment.Center)
//    )
  }
}
