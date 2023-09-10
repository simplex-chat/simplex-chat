package chat.simplex.app.views.helpers

interface ValueTitle <T> {
  val value: T
  val title: String
}

data class ValueTitleDesc <T> (
  override val value: T,
  override val title: String,
  val description: String
): ValueTitle<T>
