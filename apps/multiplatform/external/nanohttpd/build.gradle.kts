plugins {
  `java-library`
}

// Built from the upstream submodule pinned at efb2ebf85a2b06f7c508aba9eaad5377e3a01e81, because
// upstream never released the org.nanohttpd packages and JitPack no longer serves or builds that
// commit. Only the core and websocket modules are used, the samples are not.
group = "org.nanohttpd"
version = "efb2ebf"

val upstream = layout.projectDirectory.dir("upstream")

sourceSets {
  main {
    java {
      setSrcDirs(listOf(upstream.dir("core/src/main/java"), upstream.dir("websocket/src/main/java")))
      exclude("org/nanohttpd/samples/**")
    }
    resources.setSrcDirs(listOf(upstream.dir("core/src/main/resources")))
  }
}

java {
  val jvmVersion = JavaVersion.toVersion(providers.gradleProperty("kotlin.jvm.target").get())
  sourceCompatibility = jvmVersion
  targetCompatibility = jvmVersion
}

val upstreamSources = sourceSets.main.get().java.matching { include("org/nanohttpd/**") }

// compileJava and jar silently succeed without the sources, so the check needs its own task.
// It cannot run during configuration, Android builds must work without the submodule.
val checkUpstreamSources by tasks.registering {
  doFirst {
    if (upstreamSources.isEmpty) {
      throw GradleException("nanohttpd sources are missing, run: git submodule update --init --recursive")
    }
  }
}

tasks.compileJava {
  dependsOn(checkUpstreamSources)
}

// Without this the jar records the build machine's timestamps, file order and file modes,
// which makes the desktop packages unreproducible
tasks.jar {
  isPreserveFileTimestamps = false
  isReproducibleFileOrder = true
  filePermissions { unix("644") }
  dirPermissions { unix("755") }
}
