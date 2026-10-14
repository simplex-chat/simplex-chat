package chat.simplex.app.ui.harness

import java.io.File
import java.util.Properties

object UiEnv {
  private val props = Properties().also { p ->
    System.getProperty("simplex.uitest.env")?.let(::File)?.takeIf { it.exists() }?.inputStream()?.use { p.load(it) }
  }
  private fun required(key: String): String = props.getProperty(key)?.takeIf { it.isNotBlank() } ?: error("env.properties has no $key")
  val libAppLib: String = props.getProperty("libAppLib")
    ?: File("src/commonMain/cpp/desktop/libs/linux-x86_64/libapp-lib.so").absolutePath
  val runDir: File = File(props.getProperty("runDir")?.takeIf { it.isNotBlank() } ?: File("build/uitest/run-local").absolutePath)
  val jvmDir: File by lazy { File(runDir, "jvm-${TestServers.slot}-${ProcessHandle.current().pid()}") }
  val repoRoot: File get() = File(required("repoRoot"))
  val cli: String get() = required("cli")
  val servers: String get() = required("servers")
}
