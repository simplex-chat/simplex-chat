package chat.simplex.common.platform

/**
 * Offer to save a reminder for a date, using whatever the platform provides.
 *
 * Nothing renews automatically and the app sends no expiry notifications, so
 * this is how a user is actually reminded before a name lapses: the reminder
 * lives in their calendar, which survives losing the device.
 *
 * Returns false when the platform has nowhere to put it, so the caller can say
 * so rather than appearing to have done something.
 */
/**
 * @param remindDaysBefore how long before [epochSeconds] to alert. A reminder
 *   on the day itself is too late to be useful: extending needs a purchase, and
 *   a name that lapses becomes available to anyone.
 */
expect fun addCalendarReminder(title: String, description: String, epochSeconds: Long, remindDaysBefore: Int = 7): Boolean
