import java.io.File
import java.util.*

buildscript {
    val prop = java.util.Properties().apply {
        try {
            load(java.io.FileInputStream(File(rootProject.rootDir, "local.properties")))
        } catch (e: Exception) {
            // No file was created
        }
    }
    fun ExtraPropertiesExtension.getOrNull(name: String): Any? = if (has(name)) get("name") else null

    extra.set("compose.version", prop["compose.version"] ?: extra["compose.version"])
    extra.set("kotlin.version", prop["kotlin.version"] ?: extra["kotlin.version"])
    extra.set("gradle.plugin.version", prop["gradle.plugin.version"] ?: extra["gradle.plugin.version"])
    extra.set("abi_filter", prop["abi_filter"] ?: "arm64-v8a")
    // Name that will be shown for debug build. By default, it is from strings
    extra.set("app.name", prop["app.name"] ?: "@string/app_name")
    // Whether the app is debuggable or not. Specify `false` if yo`u want good performance in debug builds
    extra.set("enable_debuggable", prop["debuggable"] != "false")
    // Ending part of package name.
    // Provide, for example, `application_id.suffix=.debug` in local.properties
    // to allow debug & release versions to coexist
    extra.set("application_id.suffix", prop["application_id.suffix"] ?: "")
    // Compression level for debug AND release apk. 0 = disable compression. Max is 9
    extra.set("compression.level", (prop["compression.level"] as String?)?.toIntOrNull() ?: 0)
    // NOTE: If you need a different version of something, provide it in `local.properties`
    // like so: compose.version=123, or gradle.plugin.version=1.2.3, etc


    /** Mac signing and notarization */
    // You can specify `compose.desktop.mac.*` keys and values from the right side of the command in `$HOME/.gradle/gradle.properties`.
    // This will be project-independent setup without requiring to have `local.properties` file
    extra.set("desktop.mac.signing.identity", prop["desktop.mac.signing.identity"] ?: extra.getOrNull("compose.desktop.mac.signing.identity"))
    extra.set("desktop.mac.signing.keychain", prop["desktop.mac.signing.keychain"] ?: extra.getOrNull("compose.desktop.mac.signing.keychain"))
    extra.set("desktop.mac.notarization.apple_id", prop["desktop.mac.notarization.apple_id"] ?: extra.getOrNull("compose.desktop.mac.notarization.appleID"))
    extra.set("desktop.mac.notarization.password", prop["desktop.mac.notarization.password"] ?: extra.getOrNull("compose.desktop.mac.notarization.password"))
    extra.set("desktop.mac.notarization.team_id", prop["desktop.mac.notarization.team_id"] ?: extra.getOrNull("compose.desktop.mac.notarization.teamID"))

    repositories {
        google()
        mavenCentral()
    }
    dependencies {
        classpath("com.android.tools.build:gradle:${rootProject.extra["gradle.plugin.version"]}")
        classpath(kotlin("gradle-plugin", version = rootProject.extra["kotlin.version"] as String))
        classpath("dev.icerock.moko:resources-generator:0.23.0")

        // Workaround gradle sync issue: https://github.com/gmazzo/gradle-buildconfig-plugin/issues/137#issuecomment-1935739759
        classpath("com.squareup:kotlinpoet:1.16.0")

        // NOTE: Do not place your application dependencies here; they belong
        // in the individual module build.gradle files
    }
}// Top-level build file where you can add configuration options common to all sub-projects/modules.
/*plugins {
    id "com.android.application" version "$gradle_plugin_version" apply false
    id "com.android.library" version "$gradle_plugin_version" apply false
    id "org.jetbrains.kotlin.android" version "$kotlin_version" apply false
    id "org.jetbrains.kotlin.plugin.serialization" version "$kotlin_version"
}*/

group = "chat.simplex"
version = extra["android.version_name"] as String

allprojects {
    repositories {
        google()
        mavenCentral()
        maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
        maven("https://oss.sonatype.org/content/repositories/snapshots")
        maven("https://jitpack.io")
    }
}

plugins {
    kotlin("multiplatform") apply false
    kotlin("android") apply false
    id("com.android.application") apply false
    id("com.android.library") apply false
    id("org.jetbrains.compose") apply false
    id("org.jetbrains.kotlin.plugin.serialization") apply false
}

tasks.register("clean", Delete::class) {
    delete(rootProject.buildDir)
}
