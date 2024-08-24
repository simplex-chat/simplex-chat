package chat.simplex.common.views.helpers

import okhttp3.MediaType
import okhttp3.ResponseBody
import okio.*

// https://github.com/square/okhttp/blob/master/samples/guide/src/main/java/okhttp3/recipes/Progress.java
class ProgressResponseBody(
  val responseBody: ResponseBody,
  val progressListener: ProgressListener
): ResponseBody() {
  private var bufferedSource: BufferedSource? = null

  override fun contentType(): MediaType? {
    return responseBody.contentType()
  }

  override fun contentLength(): Long {
    return responseBody.contentLength()
  }

  override fun source(): BufferedSource {
    if (bufferedSource == null) {
      bufferedSource = source(responseBody.source()).buffer()
    }
    return bufferedSource!!
  }

  private fun source(source: Source): Source {
    return object: ForwardingSource(source) {
      var totalBytesRead = 0L

      override fun read(sink: Buffer, byteCount: Long): Long {
        val bytesRead = super.read(sink, byteCount)
        // read() returns the number of bytes read, or -1 if this source is exhausted.
        totalBytesRead += if (bytesRead != -1L) bytesRead else 0L
        progressListener.update(totalBytesRead, responseBody.contentLength(), bytesRead == -1L)
        return bytesRead;
      }
    }
  }
}

interface ProgressListener {
  fun update(bytesRead: Long, contentLength: Long, done: Boolean);
}
