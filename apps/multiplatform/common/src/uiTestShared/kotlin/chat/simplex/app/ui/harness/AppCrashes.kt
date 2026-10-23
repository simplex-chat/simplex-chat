package chat.simplex.app.ui.harness

import org.junit.AssumptionViolatedException
import org.junit.rules.TestWatcher
import org.junit.runner.Description
import java.util.concurrent.ConcurrentLinkedQueue

@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
@Repeatable
annotation class KnownAppIssue(val finding: String, val error: String, val intermittent: Boolean = false)

class KnownIssueRule : TestWatcher() {
  var issues: List<KnownAppIssue> = emptyList()
    private set

  override fun starting(description: Description) {
    issues = description.annotations.flatMap { a ->
      when {
        a is KnownAppIssue -> listOf(a)
        a.annotationClass.java.enclosingClass == KnownAppIssue::class.java -> (a.annotationClass.java.getMethod("value").invoke(a) as Array<*>).filterIsInstance<KnownAppIssue>()
        else -> emptyList()
      }
    }
  }
}

fun withKnownIssue(issues: List<KnownAppIssue>, block: () -> Unit) {
  if (issues.isEmpty()) return block()
  try {
    block()
  } catch (e: AssumptionViolatedException) {
    throw e
  } catch (e: Throwable) {
    val trace = e.stackTraceToString()
    issues.firstOrNull { it.error in trace }?.let { throw AssumptionViolatedException("known app issue (plan ${it.finding}): ${it.error}") }
    throw e
  }
  issues.firstOrNull { !it.intermittent }?.let { throw AssertionError("known app issue (plan ${it.finding}) no longer reproduces; remove @KnownAppIssue") }
  issues.forEach { println("UITEST known app issue (plan ${it.finding}) did not reproduce in this run") }
}

object AppCrashes {
  private val recorded = ConcurrentLinkedQueue<Pair<String, Throwable>>()

  fun install() {
    val current = Thread.getDefaultUncaughtExceptionHandler()
    if (current !is Recorder) Thread.setDefaultUncaughtExceptionHandler(Recorder(current))
  }

  fun clear() = recorded.clear()

  fun failIfAny() {
    if (recorded.isEmpty()) return
    val crashes = generateSequence { recorded.poll() }.toList()
    throw AssertionError("the app crashed:\n" + crashes.joinToString("\n") { (thread, e) -> "thread $thread: " + e.stackTraceToString().lines().take(25).joinToString("\n") })
  }

  private class Recorder(private val next: Thread.UncaughtExceptionHandler?) : Thread.UncaughtExceptionHandler {
    override fun uncaughtException(t: Thread, e: Throwable) {
      recorded.add(t.name to e)
      if (next != null) next.uncaughtException(t, e) else e.printStackTrace()
    }
  }
}

fun failOnPeerErrors(peers: List<PeerLines>) {
  val errors = peers.flatMap { p -> p.unexpectedErrors().map { "${p.name}: $it" } }
  if (errors.isNotEmpty()) throw AssertionError("peers printed errors the test did not expect:\n" + errors.joinToString("\n"))
}
