package chat.simplex.app.views.helpers

import androidx.compose.runtime.Composable
import chat.simplex.app.model.LinkPreview

suspend fun getLinkPreview(url: String): LinkPreview? {
  withApi {  }
}



@Composable
fun ChatItemLinkPreview(metadata: LinkPreview) {

}

@Composable
fun ComposeLinkPreview(metadata: LinkPreview, cancelPreview: () -> Unit) {

}