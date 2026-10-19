package chat.simplex.app.ui.harness

import org.junit.AssumptionViolatedException
import org.junit.rules.TestWatcher
import org.junit.runner.Description
import java.util.concurrent.ConcurrentLinkedQueue

@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class KnownAppIssue(val finding: String, val error: String, val intermittent: Boolean = false)

class KnownIssueRule : TestWatcher() {
  var issue: KnownAppIssue? = null
    private set

  override fun starting(description: Description) {
    issue = description.getAnnotation(KnownAppIssue::class.java)
  }
}

fun withKnownIssue(issue: KnownAppIssue?, block: () -> Unit) {
  if (issue == null) return block()
  try {
    block()
  } catch (e: AssumptionViolatedException) {
    throw e
  } catch (e: Throwable) {
    if (issue.error in e.stackTraceToString()) throw AssumptionViolatedException("known app issue (plan ${issue.finding}): ${issue.error}")
    throw e
  }
  if (!issue.intermittent) throw AssertionError("known app issue (plan ${issue.finding}) no longer reproduces; remove @KnownAppIssue")
  println("UITEST known app issue (plan ${issue.finding}) did not reproduce in this run")
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
