import org.jetbrains.compose.desktop.application.dsl.TargetFormat

plugins {
  kotlin("multiplatform")
  id("org.jetbrains.compose")
  id("io.github.tomtzook.gradle-cmake") version "1.2.2"
}

group = "chat.simplex"
version = extra["desktop.version_name"] as String


kotlin {
  jvm {
    jvmToolchain(11)
    withJava()
  }
  sourceSets {
    val jvmMain by getting {
      dependencies {
        implementation(project(":common"))
        implementation(compose.desktop.currentOs)
      }
    }
    val jvmTest by getting
  }
}

// https://github.com/JetBrains/compose-multiplatform/tree/master/tutorials/Native_distributions_and_local_execution
compose {
  desktop {
    application {
      // For debugging via VisualVM
      /*jvmArgs += listOf(
        "-Dcom.sun.management.jmxremote.port=8080",
        "-Dcom.sun.management.jmxremote.ssl=false",
        "-Dcom.sun.management.jmxremote.authenticate=false"
      )*/
      mainClass = "chat.simplex.desktop.MainKt"
      nativeDistributions {
        // For debugging via VisualVM
        //modules("jdk.zipfs", "jdk.management.agent")
        modules("jdk.zipfs")
        //includeAllModules = true
        outputBaseDir.set(project.file("../release"))
        targetFormats(
          TargetFormat.Deb, TargetFormat.Dmg, TargetFormat.Msi, TargetFormat.Exe
          //, TargetFormat.AppImage // Gradle doesn't sync on Mac with it
        )
        linux {
          iconFile.set(project.file("src/jvmMain/resources/distribute/simplex.png"))
          appCategory = "Messenger"
        }
        windows {
          packageName = "SimpleX"
          iconFile.set(project.file("src/jvmMain/resources/distribute/simplex.ico"))
          console = true
          perUserInstall = true
          dirChooser = true
        }
        macOS {
          packageName = "SimpleX"
          iconFile.set(project.file("src/jvmMain/resources/distribute/simplex.icns"))
          appCategory = "public.app-category.social-networking"
          bundleID = "chat.simplex.app"
        }
        packageName = "simplex"
        // Packaging requires to have version like MAJOR.MINOR.PATCH
        var adjustedVersion = rootProject.extra["desktop.version_name"] as String
        adjustedVersion = adjustedVersion.replace(Regex("[^0-9.]"), "")
        if (adjustedVersion.split(".").size != 3) {
          adjustedVersion += ".0"
        }
        version = adjustedVersion
      }
    }
  }
}

val cppPath = "../common/src/commonMain/cpp"
cmake {
  // Run this command to make build for all targets:
  // ./gradlew common:cmakeBuild -PcrossCompile
  if (project.hasProperty("crossCompile")) {
    machines.customMachines.register("linux-amd64") {
      toolchainFile.set(project.file("$cppPath/toolchains/x86_64-linux-gnu-gcc.cmake"))
    }
    /*machines.customMachines.register("linux-aarch64") {
      toolchainFile.set(project.file("$cppPath/toolchains/aarch64-linux-gnu-gcc.cmake"))
    }*/
    machines.customMachines.register("win-amd64") {
      toolchainFile.set(project.file("$cppPath/toolchains/x86_64-windows-mingw32-gcc.cmake"))
    }
    if (machines.host.name == "mac-amd64") {
      machines.customMachines.register("mac-amd64") {
        toolchainFile.set(project.file("$cppPath/toolchains/x86_64-mac-apple-darwin-gcc.cmake"))
      }
    }
    if (machines.host.name == "mac-aarch64") {
      machines.customMachines.register("mac-aarch64") {
        toolchainFile.set(project.file("$cppPath/toolchains/aarch64-mac-apple-darwin-gcc.cmake"))
      }
    }
  }
  val compileMachineTargets = arrayListOf<com.github.tomtzook.gcmake.targets.TargetMachine>(machines.host)
  compileMachineTargets.addAll(machines.customMachines)
  targets {
    val main by creating {
      cmakeLists.set(file("$cppPath/desktop/CMakeLists.txt"))
      targetMachines.addAll(compileMachineTargets.toSet())
    }
  }
}

tasks.named("clean") {
  dependsOn("cmakeClean")
}
tasks.named("compileJava") {
  dependsOn("cmakeBuildAndCopy")
}
afterEvaluate {
  tasks.create("cmakeBuildAndCopy") {
    dependsOn("cmakeBuild")
    doLast {
      copy {
        from("${project(":desktop").buildDir}/cmake/main/linux-amd64", "$cppPath/desktop/libs/linux-x86_64", "$cppPath/desktop/libs/linux-x86_64/deps")
        into("src/jvmMain/resources/libs/linux-x86_64")
        include("*.so*")
        eachFile {
          path = name
        }
        includeEmptyDirs = false
        duplicatesStrategy = DuplicatesStrategy.INCLUDE
      }
      copy {
        from("${project(":desktop").buildDir}/cmake/main/linux-aarch64", "$cppPath/desktop/libs/linux-aarch64", "$cppPath/desktop/libs/linux-aarch64/deps")
        into("src/jvmMain/resources/libs/linux-aarch64")
        include("*.so*")
        eachFile {
          path = name
        }
        includeEmptyDirs = false
        duplicatesStrategy = DuplicatesStrategy.INCLUDE
      }
      copy {
        from("${project(":desktop").buildDir}/cmake/main/win-amd64", "$cppPath/desktop/libs/windows-x86_64", "$cppPath/desktop/libs/windows-x86_64/deps")
        into("src/jvmMain/resources/libs/windows-x86_64")
        include("*.dll")
        eachFile {
          path = name
        }
        includeEmptyDirs = false
        duplicatesStrategy = DuplicatesStrategy.INCLUDE
      }
      copy {
        from("${project(":desktop").buildDir}/cmake/main/mac-x86_64", "$cppPath/desktop/libs/mac-x86_64", "$cppPath/desktop/libs/mac-x86_64/deps")
        into("src/jvmMain/resources/libs/mac-x86_64")
        include("*.dylib")
        eachFile {
          path = name
        }
        includeEmptyDirs = false
        duplicatesStrategy = DuplicatesStrategy.INCLUDE
      }
      copy {
        from("${project(":desktop").buildDir}/cmake/main/mac-aarch64", "$cppPath/desktop/libs/mac-aarch64", "$cppPath/desktop/libs/mac-aarch64/deps")
        into("src/jvmMain/resources/libs/mac-aarch64")
        include("*.dylib")
        eachFile {
          path = name
        }
        includeEmptyDirs = false
        duplicatesStrategy = DuplicatesStrategy.INCLUDE
      }
    }
  }
}
