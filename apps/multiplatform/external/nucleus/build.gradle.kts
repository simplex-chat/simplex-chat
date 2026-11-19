import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
  kotlin("jvm")
  id("org.jetbrains.compose")
  id("org.jetbrains.kotlin.plugin.compose")
}

group = "dev.nucleusframework"
version = "bb34fbc"

val upstream = layout.projectDirectory.dir("upstream")
val composeVersion = rootProject.extra["compose.version"] as String
val nucleusModules = listOf(
  "core-runtime",
  "energy-manager",
  "freedesktop-icons",
  "decorated-window-core",
  "decorated-window-tao",
  "darkmode-detector",
  "notification-linux",
  "launcher-linux",
)

val composeStubs: SourceSet by sourceSets.creating {
  java.setSrcDirs(listOf(upstream.dir("decorated-window-tao/src/composeStubs/java")))
}

sourceSets {
  main {
    kotlin.setSrcDirs(nucleusModules.flatMap { listOf(upstream.dir("$it/src/main/kotlin"), upstream.dir("$it/src/main/java")) })
    java.setSrcDirs(nucleusModules.map { upstream.dir("$it/src/main/java") })
    resources.setSrcDirs(nucleusModules.map { upstream.dir("$it/src/main/resources") })
    resources.exclude("nucleus/native/**")
  }
}

dependencies {
  compileOnly(composeStubs.output)
  api("org.jetbrains.compose.foundation:foundation:$composeVersion")
  api("org.jetbrains.compose.desktop:desktop:$composeVersion")
  implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.11.0")
  compileOnly("androidx.lifecycle:lifecycle-runtime:2.11.0")
  compileOnly("androidx.lifecycle:lifecycle-viewmodel-savedstate:2.11.0")
  compileOnly("androidx.navigationevent:navigationevent:1.1.1")
  compileOnly("org.jetbrains.androidx.lifecycle:lifecycle-viewmodel-compose:2.11.0")
  compileOnly("org.jetbrains.compose.hot-reload:hot-reload-agent:1.2.0")
  compileOnly("org.jetbrains.compose.hot-reload:hot-reload-core:1.2.0")
  compileOnly("org.jetbrains.compose.hot-reload:hot-reload-orchestration:1.2.0")
  compileOnly("org.jetbrains.compose.hot-reload:hot-reload-devtools-api:1.2.0")
}

java {
  val jvmVersion = JavaVersion.toVersion(providers.gradleProperty("desktop.jvm.target").get())
  sourceCompatibility = jvmVersion
  targetCompatibility = jvmVersion
}

kotlin {
  compilerOptions {
    jvmTarget.set(JvmTarget.fromTarget(providers.gradleProperty("desktop.jvm.target").get()))
    optIn.add("dev.nucleusframework.window.ExperimentalNucleusApi")
  }
}

val upstreamSources = sourceSets.main.get().allSource.matching { include("dev/nucleusframework/**") }

val checkUpstreamSources by tasks.registering {
  doFirst {
    if (upstreamSources.isEmpty) {
      throw GradleException("Nucleus sources are missing, run: git submodule update --init --recursive")
    }
  }
}

tasks.compileKotlin {
  dependsOn(checkUpstreamSources)
}

tasks.jar {
  isPreserveFileTimestamps = false
  isReproducibleFileOrder = true
  filePermissions { unix("644") }
  dirPermissions { unix("755") }
}
