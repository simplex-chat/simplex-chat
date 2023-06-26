package chat.simplex.common.platform

import android.util.Log

actual object Log {
  actual fun d(tag: String, text: String) = Log.d(tag, text).run{}
  actual fun e(tag: String, text: String) = Log.e(tag, text).run{}
  actual fun i(tag: String, text: String) = Log.i(tag, text).run{}
  actual fun w(tag: String, text: String) = Log.w(tag, text).run{}
}