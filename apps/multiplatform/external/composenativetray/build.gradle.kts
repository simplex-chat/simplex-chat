import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
  kotlin("jvm")
  id("org.jetbrains.compose")
  id("org.jetbrains.kotlin.plugin.compose")
}

group = "dev.nucleusframework"
version = "05e381c"

val upstream = layout.projectDirectory.dir("upstream")
val composeVersion = rootProject.extra["compose.version"] as String

sourceSets {
  main {
    kotlin.setSrcDirs(listOf(upstream.dir("src/jvmMain/kotlin")))
    resources.setSrcDirs(listOf(upstream.dir("src/jvmMain/resources")))
    resources.exclude("composetray/native/**")
  }
}

dependencies {
  implementation(project(":external:nucleus"))
  implementation("org.jetbrains.compose.runtime:runtime:$composeVersion")
  implementation("org.jetbrains.compose.foundation:foundation:$composeVersion")
  implementation("org.jetbrains.compose.ui:ui:$composeVersion")
  implementation("org.jetbrains.compose.components:components-resources:$composeVersion")
  implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.11.0")
}

java {
  val jvmVersion = JavaVersion.toVersion(providers.gradleProperty("desktop.jvm.target").get())
  sourceCompatibility = jvmVersion
  targetCompatibility = jvmVersion
}

kotlin {
  compilerOptions {
    jvmTarget.set(JvmTarget.fromTarget(providers.gradleProperty("desktop.jvm.target").get()))
  }
}

val upstreamSources = sourceSets.main.get().allSource.matching { include("dev/nucleusframework/**") }

val checkUpstreamSources = tasks.register("checkUpstreamSources") {
  doFirst {
    if (upstreamSources.isEmpty) {
      throw GradleException("ComposeNativeTray sources are missing, run: git submodule update --init --recursive")
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
