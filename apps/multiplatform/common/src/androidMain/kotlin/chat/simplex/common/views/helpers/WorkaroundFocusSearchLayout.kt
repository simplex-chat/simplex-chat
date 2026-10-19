package chat.simplex.common.views.helpers

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.FrameLayout

/**
 * A workaround for the ANR issue on Compose 1.7.x.
 * https://issuetracker.google.com/issues/369354336
 * Code from:
 * https://issuetracker.google.com/issues/369354336#comment8
*/
class WorkaroundFocusSearchLayout : FrameLayout {

  constructor(
    context: Context,
  ) : super(context)

  constructor(
    context: Context,
    attrs: AttributeSet?,
  ) : super(context, attrs)

  constructor(
    context: Context,
    attrs: AttributeSet?,
    defStyleAttr: Int,
  ) : super(context, attrs, defStyleAttr)

  constructor(
    context: Context,
    attrs: AttributeSet?,
    defStyleAttr: Int,
    defStyleRes: Int,
  ) : super(context, attrs, defStyleAttr, defStyleRes)

  override fun focusSearch(focused: View?, direction: Int): View? {
    return null
  }
}