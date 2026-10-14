package chat.simplex.common.ui

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

object UiIdle {
  class Item(val id: Long, val what: String)

  private val items = ConcurrentHashMap<Long, Item>()
  private val abandoned = ConcurrentHashMap.newKeySet<Long>()
  private val nextId = AtomicLong()
  @Volatile var lastEventAt = 0L
    private set

  fun start(what: () -> String): Long {
    if (!uiTestHooksEnabled) return 0
    val id = nextId.incrementAndGet()
    items[id] = Item(id, what())
    return id
  }

  fun finish(id: Long) {
    if (id == 0L) return
    items.remove(id)
    abandoned.remove(id)
  }

  inline fun <T> track(noinline what: (() -> String)?, block: () -> T): T {
    val id = if (what == null) 0 else start(what)
    try {
      return block()
    } finally {
      finish(id)
    }
  }

  inline fun <T> trackEvent(noinline what: () -> String, block: () -> T): T =
    try {
      track(what, block)
    } finally {
      if (uiTestHooksEnabled) eventProcessed()
    }

  fun eventProcessed() {
    lastEventAt = System.currentTimeMillis()
  }

  fun inFlight(): List<Item> = items.values.filter { it.id !in abandoned }.sortedBy { it.id }

  fun abandon(stuck: List<Item>) {
    stuck.forEach { abandoned.add(it.id) }
  }
}
