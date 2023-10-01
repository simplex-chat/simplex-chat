package chat.simplex.common.views.helpers

import androidx.compose.runtime.*
import androidx.compose.ui.graphics.ImageBitmap
import java.net.URI

@Composable
expect fun GetImageBottomSheet(
  imageBitmap: MutableState<URI?>,
  onImageChange: (ImageBitmap) -> Unit,
  hideBottomSheet: () -> Unit
)

