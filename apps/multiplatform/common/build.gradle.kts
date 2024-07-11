plugins {
  kotlin("multiplatform")
  id("org.jetbrains.compose")
  id("com.android.library")
  id("org.jetbrains.kotlin.plugin.serialization")
  id("dev.icerock.mobile.multiplatform-resources")
  id("com.github.gmazzo.buildconfig") version "5.3.5"
}

group = "chat.simplex"
version = extra["android.version_name"] as String

kotlin {
  androidTarget()
  jvm("desktop")
  applyDefaultHierarchyTemplate()
  sourceSets {
    all {
      languageSettings {
        optIn("kotlinx.coroutines.DelicateCoroutinesApi")
        optIn("androidx.compose.foundation.ExperimentalFoundationApi")
        optIn("androidx.compose.ui.text.ExperimentalTextApi")
        optIn("androidx.compose.material.ExperimentalMaterialApi")
        optIn("com.arkivanov.decompose.ExperimentalDecomposeApi")
        optIn("kotlinx.serialization.InternalSerializationApi")
        optIn("kotlinx.serialization.ExperimentalSerializationApi")
        optIn("androidx.compose.ui.ExperimentalComposeUiApi")
        optIn("com.google.accompanist.permissions.ExperimentalPermissionsApi")
      }
    }

    val commonMain by getting {
      dependencies {
        api(compose.runtime)
        api(compose.foundation)
        api(compose.material)
        api("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.3")
        api("org.jetbrains.kotlinx:kotlinx-datetime:0.5.0")
        api("com.russhwolf:multiplatform-settings:1.1.1")
        api("com.charleskorn.kaml:kaml:0.59.0")
        api("org.jetbrains.compose.ui:ui-text:${rootProject.extra["compose.version"] as String}")
        implementation("org.jetbrains.compose.components:components-animatedimage:${rootProject.extra["compose.version"] as String}")
        //Barcode
        api("org.boofcv:boofcv-core:1.1.3")
        implementation("com.godaddy.android.colorpicker:compose-color-picker-jvm:0.7.0")
        // Link Previews
        implementation("org.jsoup:jsoup:1.17.2")
        // Resources
        api("dev.icerock.moko:resources:0.23.0")
        api("dev.icerock.moko:resources-compose:0.23.0")
      }
    }
    val commonTest by getting {
      dependencies {
        implementation(kotlin("test"))
        implementation(kotlin("test-junit"))
        implementation(kotlin("test-common"))
        implementation(kotlin("test-annotations-common"))
      }
    }
    val androidMain by getting {
      kotlin.srcDir("build/generated/moko/androidMain/src")
      dependencies {
        implementation("androidx.activity:activity-compose:1.8.2")
        val workVersion = "2.9.0"
        implementation("androidx.work:work-runtime-ktx:$workVersion")
        implementation("com.google.accompanist:accompanist-insets:0.30.1")

        // Video support
        implementation("com.google.android.exoplayer:exoplayer:2.19.1")

        // Biometric authentication
        implementation("androidx.biometric:biometric:1.2.0-alpha05")

        //Barcode
        implementation("org.boofcv:boofcv-android:1.1.3")

        //Camera Permission
        implementation("com.google.accompanist:accompanist-permissions:0.34.0")

        implementation("androidx.webkit:webkit:1.10.0")

        // GIFs support
        implementation("io.coil-kt:coil-compose:2.6.0")
        implementation("io.coil-kt:coil-gif:2.6.0")

        implementation("com.jakewharton:process-phoenix:2.2.0")

        val cameraXVersion = "1.3.2"
        implementation("androidx.camera:camera-core:${cameraXVersion}")
        implementation("androidx.camera:camera-camera2:${cameraXVersion}")
        implementation("androidx.camera:camera-lifecycle:${cameraXVersion}")
        implementation("androidx.camera:camera-view:${cameraXVersion}")

        // Calls lifecycle listener
        implementation("androidx.lifecycle:lifecycle-process:2.4.1")
      }
    }
    val desktopMain by getting {
      dependencies {
        implementation("org.jetbrains.kotlinx:kotlinx-coroutines-swing:1.8.0")
        implementation("com.github.Dansoftowner:jSystemThemeDetector:3.8")
        implementation("com.sshtools:two-slices:0.9.0-SNAPSHOT")
        implementation("org.slf4j:slf4j-simple:2.0.12")
        implementation("uk.co.caprica:vlcj:4.8.2")
        implementation("com.github.NanoHttpd.nanohttpd:nanohttpd:efb2ebf85a")
        implementation("com.github.NanoHttpd.nanohttpd:nanohttpd-websocket:efb2ebf85a")
        implementation("com.squareup.okhttp3:okhttp:4.12.0")
      }
    }
    val desktopTest by getting
  }
}

android {
  namespace = "chat.simplex.common"
  compileSdk = 34
  sourceSets["main"].manifest.srcFile("src/androidMain/AndroidManifest.xml")
  defaultConfig {
    minSdk = 26
  }
  testOptions.targetSdk = 33
  lint.targetSdk = 33
  val isAndroid = gradle.startParameter.taskNames.find {
    val lower = it.lowercase()
    lower.contains("release") || lower.startsWith("assemble") || lower.startsWith("install")
  } != null
  if (isAndroid) {
    // This is not needed on Android but can't be moved to desktopMain because MR lib don't support this.
    // No other ways to exclude a file work, but it's large and should be excluded
    kotlin.sourceSets["commonMain"].resources.exclude("/MR/fonts/NotoColorEmoji-Regular.ttf")
  }
}

multiplatformResources {
  multiplatformResourcesPackage = "chat.simplex.res"
  //  multiplatformResourcesClassName = "MR"
}

buildConfig {
  forClass("BuildConfigCommon") {
    buildConfigField("String", "ANDROID_VERSION_NAME", "\"${extra["android.version_name"]}\"")
    buildConfigField("int", "ANDROID_VERSION_CODE", "${extra["android.version_code"]}")
    buildConfigField("String", "DESKTOP_VERSION_NAME", "\"${extra["desktop.version_name"]}\"")
    buildConfigField("int", "DESKTOP_VERSION_CODE", "${extra["desktop.version_code"]}")
  }
}

afterEvaluate {
  tasks.named("generateMRcommonMain") {
    dependsOn("adjustFormatting")
  }
  tasks.create("adjustFormatting") {
    doLast {
      val debug = false
      val stringRegex = Regex(".*<string .*</string>.*")
      val startStringRegex = Regex("<string [^>]*>")
      val endStringRegex = Regex("</string>[ ]*")
      val endTagRegex = Regex("</")
      val anyHtmlRegex = Regex("[^>]*>.*(<|>).*</string>|[^>]*>.*(&lt;|&gt;).*</string>")
      val fontLtGtRegex = Regex("[^>]*>.*&lt;font[^>]*&gt;.*&lt;/font&gt;.*</string>")
      val unbracketedColorRegex = Regex("color=#[abcdefABCDEF0-9]{3,6}")
      val correctHtmlRegex = Regex("[^>]*>.*<b>.*</b>.*</string>|[^>]*>.*<i>.*</i>.*</string>|[^>]*>.*<u>.*</u>.*</string>|[^>]*>.*<font[^>]*>.*</font>.*</string>")
      val possibleFormat = listOf("s", "d", "1\$s", "2\$s", "3\$s", "4\$s", "1\$d", "2\$d", "3\$d", "4\$d", "2s", "f")

      fun String.id(): String = replace("<string name=\"", "").trim().substringBefore("\"")

      fun String.formatting(filepath: String): List<String> {
        if (!contains("%")) return emptyList()
        val value = substringAfter("\">").substringBeforeLast("</string>")

        val formats = ArrayList<String>()
        var substring = value.substringAfter("%")
        while (true) {
          var foundFormat = false
          for (format in possibleFormat) {
            if (substring.startsWith(format)) {
              formats.add(format)
              foundFormat = true
              break
            }
          }
          if (!foundFormat) {
            throw Exception("Unknown formatting in string. Add it to 'possibleFormat' in common/build.gradle.kts if needed: $this \nin $filepath")
          }
          val was = substring
          substring = substring.substringAfter("%")
          if (was.length == substring.length) break
        }
        return if (formats.any { it.startsWith("1$") || it.startsWith("2$") || it.startsWith("3$") || it.startsWith("4$") }) {
          formats.sortedBy { it.trim('s', 'd', 'f', '$').toIntOrNull() ?: throw Exception("Formatting don't have positional arguments: $this \nin $filepath") }
        } else {
          formats
        }
      }

      fun String.removeCDATA(): String =
        if (contains("<![CDATA")) {
          replace("<![CDATA[", "").replace("]]></string>", "</string>")
        } else {
          this
        }

      fun String.addCDATA(filepath: String): String {
        //return this
        if (anyHtmlRegex.matches(this)) {
          val prepared = if (fontLtGtRegex.matches(this) || unbracketedColorRegex.containsMatchIn(this)) {
            replace("&lt;", "<").replace("&gt;", ">").replace(unbracketedColorRegex) { it.value.replace("color=#", "color=\"#") + "\"" }
          } else this
          val countOfStartTag = prepared.count { it == '<' }
          val countOfEndTag = prepared.count { it == '>' }
          if (countOfStartTag != countOfEndTag || countOfStartTag != endTagRegex.findAll(prepared).count() * 2 || !correctHtmlRegex.matches(prepared)) {
            if (debug) {
              println("Wrong string:")
              println(this)
              println("in $filepath")
              println("   ")
            } else {
              throw Exception("Wrong string: $this \nin $filepath")
            }
          }
          val res = prepared.replace(startStringRegex) { it.value + "<![CDATA[" }.replace(endStringRegex) { "]]>" + it.value }
          if (debug) {
            println("Changed string:")
            println(this)
            println(res)
            println("   ")
          }
          return res
        }
        if (debug) {
          println("Correct string:")
          println(this)
          println("   ")
        }
        return this
      }
      val fileRegex = Regex("MR/../strings.xml$|MR/..-.../strings.xml$|MR/..-../strings.xml$|MR/base/strings.xml$")
      val tree = kotlin.sourceSets["commonMain"].resources.filter { fileRegex.containsMatchIn(it.absolutePath.replace("\\", "/")) }.asFileTree
      val baseStringsFile = tree.firstOrNull { it.absolutePath.replace("\\", "/").endsWith("base/strings.xml") } ?: throw Exception("No base/strings.xml found")
      val treeList = ArrayList(tree.toList())
      treeList.remove(baseStringsFile)
      treeList.add(0, baseStringsFile)
      val baseFormatting = mutableMapOf<String, List<String>>()
      treeList.forEachIndexed { index, file ->
        val isBase = index == 0
        val initialLines = ArrayList<String>()
        val finalLines = ArrayList<String>()
        val errors = ArrayList<String>()

        file.useLines { lines ->
          val multiline = ArrayList<String>()
          lines.forEach { line ->
            initialLines.add(line)
            if (stringRegex.matches(line)) {
              val fixedLine = line.removeCDATA().addCDATA(file.absolutePath)
              val lineId = fixedLine.id()
              if (isBase) {
                baseFormatting[lineId] = fixedLine.formatting(file.absolutePath)
              } else if (baseFormatting[lineId] != fixedLine.formatting(file.absolutePath)) {
                errors.add("Incorrect formatting in string: $fixedLine \nin ${file.absolutePath}")
              }
              finalLines.add(fixedLine)
            } else if (multiline.isEmpty() && startStringRegex.containsMatchIn(line)) {
              multiline.add(line)
            } else if (multiline.isNotEmpty() && endStringRegex.containsMatchIn(line)) {
              multiline.add(line)
              val fixedLines = multiline.joinToString("\n").removeCDATA().addCDATA(file.absolutePath).split("\n")
              val fixedLinesJoined = fixedLines.joinToString("")
              val lineId = fixedLinesJoined.id()
              if (isBase) {
                baseFormatting[lineId] = fixedLinesJoined.formatting(file.absolutePath)
              } else if (baseFormatting[lineId] != fixedLinesJoined.formatting(file.absolutePath)) {
                errors.add("Incorrect formatting in string: $fixedLinesJoined \nin ${file.absolutePath}")
              }
              finalLines.addAll(fixedLines)
              multiline.clear()
            } else if (multiline.isNotEmpty()) {
              multiline.add(line)
            } else {
              finalLines.add(line)
            }
          }
          if (multiline.isNotEmpty()) {
            errors.add("Unclosed string tag: ${multiline.joinToString("\n")} \nin ${file.absolutePath}")
          }
        }

        if (errors.isNotEmpty()) {
          throw Exception("Found errors: \n\n${errors.joinToString("\n\n")}")
        }

        if (!debug && finalLines != initialLines) {
          file.writer().use {
            finalLines.forEachIndexed { index, line ->
              it.write(line)
              if (index != finalLines.lastIndex) {
                it.write("\n")
              }
            }
          }
        }
      }
    }
  }
}
