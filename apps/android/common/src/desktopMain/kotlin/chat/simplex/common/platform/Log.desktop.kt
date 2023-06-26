package chat.simplex.common.platform

actual object Log {
  actual fun d(tag: String, text: String) = println("D: $text")
  actual fun e(tag: String, text: String) = println("E: $text")
  actual fun i(tag: String, text: String) = println("I: $text")
  actual fun w(tag: String, text: String) = println("W: $text")
}