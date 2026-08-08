package chat.simplex.common.platform

// Desktop has no single calendar entry point worth guessing at; the date is
// shown on screen and can be noted by hand.
actual fun addCalendarReminder(title: String, description: String, epochSeconds: Long, remindDaysBefore: Int): Boolean = false
