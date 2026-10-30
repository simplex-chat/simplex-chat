package chat.simplex.app.ui.harness

import chat.simplex.common.platform.appPlatform
import org.junit.AssumptionViolatedException
import org.junit.rules.TestRule
import org.junit.runner.Description
import org.junit.runners.model.Statement
import java.io.File
import java.util.concurrent.ConcurrentLinkedQueue

@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
@Repeatable
annotation class KnownAppIssue(val finding: String, val error: String, val intermittent: Boolean = false, val androidOnly: Boolean = false, val desktopOnly: Boolean = false)

class KnownIssueRule : TestRule {
  override fun apply(base: Statement, description: Description): Statement = object : Statement() {
    override fun evaluate() {
      CurrentTest.name = "${description.testClass?.simpleName ?: description.className}.${description.methodName}"
      withKnownIssue("${description.className}.${description.methodName}", knownIssues(description)) { base.evaluate() }
    }
  }

  private fun knownIssues(description: Description): List<KnownAppIssue> = knownIssuesOf(description).filter { (!it.androidOnly || appPlatform.isAndroid) && (!it.desktopOnly || appPlatform.isDesktop) }

  private fun knownIssuesOf(description: Description): List<KnownAppIssue> = description.annotations.flatMap { a ->
    when {
      a is KnownAppIssue -> listOf(a)
      a.annotationClass.java.enclosingClass == KnownAppIssue::class.java -> (a.annotationClass.java.getMethod("value").invoke(a) as Array<*>).filterIsInstance<KnownAppIssue>()
      else -> emptyList()
    }
  }
}

object CurrentTest {
  @Volatile var name: String = "unknown"
}

fun withKnownIssue(test: String, issues: List<KnownAppIssue>, block: () -> Unit) {
  if (issues.isEmpty()) return block()
  try {
    block()
  } catch (e: AssumptionViolatedException) {
    throw e
  } catch (e: Throwable) {
    val message = ((e as? FailureWithSuppressed)?.primary ?: e).message.orEmpty()
    issues.firstOrNull { it.error in message }?.let {
      KnownIssueLog.write(test, "SKIPPED", it.finding, message)
      throw AssumptionViolatedException("known app issue (plan ${it.finding}): ${it.error}; failure: $message")
    }
    throw e
  }
  issues.firstOrNull { !it.intermittent }?.let { throw AssertionError("known app issue (plan ${it.finding}) no longer reproduces; remove @KnownAppIssue") }
  issues.forEach { KnownIssueLog.write(test, "NOT-REPRODUCED", it.finding, it.error) }
}

object KnownIssueLog {
  @Volatile var file: File? = null
  private const val MESSAGE_MAX = 2000

  @Synchronized fun write(test: String, result: String, finding: String, message: String) {
    val line = listOf(test, result, finding, message.replace(Regex("\\s+"), " ").take(MESSAGE_MAX)).joinToString("\t")
    println("KNOWN_ISSUE $line")
    file?.let { f -> runCatching { f.parentFile?.mkdirs(); f.appendText("$line\n") }.onFailure { println("UITEST cannot write $f: $it") } }
  }
}

class FailureWithSuppressed(val primary: Throwable) : AssertionError(
  "${primary.message}; suppressed:\n" + primary.suppressed.joinToString("\n") { it.stackTraceToString().lines().take(25).joinToString("\n") },
  primary
)

object AppCrashes {
  private val recorded = ConcurrentLinkedQueue<Pair<String, Throwable>>()

  fun install() {
    val current = Thread.getDefaultUncaughtExceptionHandler()
    if (current !is Recorder) Thread.setDefaultUncaughtExceptionHandler(Recorder(current))
  }

  fun clear() = recorded.clear()

  fun failIfAny(what: String = "the app crashed") {
    if (recorded.isEmpty()) return
    val crashes = generateSequence { recorded.poll() }.toList()
    throw AssertionError("$what:\n" + crashes.joinToString("\n") { (thread, e) -> "thread $thread: " + e.stackTraceToString().lines().take(25).joinToString("\n") })
  }

  private class Recorder(private val next: Thread.UncaughtExceptionHandler?) : Thread.UncaughtExceptionHandler {
    override fun uncaughtException(t: Thread, e: Throwable) {
      recorded.add(t.name to e)
      if (next != null) next.uncaughtException(t, e) else e.printStackTrace()
    }
  }
}

object LeakedWork {
  private val abandoned = ConcurrentLinkedQueue<String>()

  fun record(stuck: List<String>) { abandoned.addAll(stuck) }

  fun failIfAny(what: String) {
    if (abandoned.isEmpty()) return
    throw AssertionError("$what ${abandoned.distinct().joinToString()}: abandoned app work can still change the app, so every later test in this process fails")
  }
}

fun failOnPeerErrors(peers: List<PeerLines>) {
  val errors = peers.flatMap { p -> p.unexpectedErrors().map { "${p.name}: $it" } }
  if (errors.isNotEmpty()) throw AssertionError("peers printed errors the test did not expect:\n" + errors.joinToString("\n"))
}
