@file:Suppress("UnstableApiUsage")

import java.net.URI
import java.net.HttpURLConnection

plugins {
    id("com.android.application")
    id("org.jetbrains.compose")
    kotlin("android")
    id("org.jetbrains.kotlin.plugin.serialization")
    id("org.jetbrains.kotlin.plugin.compose")
}

// Добавьте этот блок сюда:
repositories {
    google()
    mavenCentral()
    maven { url = uri("https://jitpack.io") }
}

android {
    compileSdk = 36

    defaultConfig {
        applicationId = "chat.simplex.app"
        namespace = "chat.simplex.app"
        minSdk = 26
        targetSdk = 36
        // !!!
        // skip version code after release to F-Droid, as it uses two version codes
        versionCode = (extra["android.version_code"] as String).toInt()
        versionName = "7.1.3 Caramel Donut"

        testInstrumentationRunner = "android.support.test.runner.AndroidJUnitRunner"
        vectorDrawables {
            useSupportLibrary = true
        }
        externalNativeBuild {
            cmake {
                cppFlags("")
            }
        }
        manifestPlaceholders["app_name"] = "@string/app_name"
        manifestPlaceholders["provider_authorities"] = "chat.simplex.app.provider"
        manifestPlaceholders["extract_native_libs"] = rootProject.extra["compression.level"] as Int != 0
    }

    // `google` is distributed via Google Play as an app bundle and includes Play Billing.
    // `foss` is distributed via F-Droid and as APKs on GitHub, without Play dependencies.
    flavorDimensions += "store"
    productFlavors {
        create("google") {
            dimension = "store"
            buildConfigField("boolean", "PLAY_STORE", "true")
        }
        create("foss") {
            dimension = "store"
            isDefault = true
            buildConfigField("boolean", "PLAY_STORE", "false")
        }
    }

    buildTypes {
        debug {
            applicationIdSuffix = rootProject.extra["application_id.suffix"] as String
            isDebuggable = rootProject.extra["enable_debuggable"] as Boolean
            manifestPlaceholders["app_name"] = rootProject.extra["app.name"] as String
            // Provider can"t be the same for different apps on the same device
            manifestPlaceholders["provider_authorities"] = "chat.simplex.app${rootProject.extra["application_id.suffix"]}.provider"
        }
        release {
            isMinifyEnabled = false
            proguardFiles(getDefaultProguardFile("proguard-android-optimize.txt"), "proguard-rules.pro")
        }
    }
    kotlinOptions {
        freeCompilerArgs += "-opt-in=kotlinx.coroutines.DelicateCoroutinesApi"
        freeCompilerArgs += "-opt-in=androidx.compose.foundation.ExperimentalFoundationApi"
        freeCompilerArgs += "-opt-in=androidx.compose.ui.text.ExperimentalTextApi"
        freeCompilerArgs += "-opt-in=androidx.compose.material.ExperimentalMaterialApi"
        freeCompilerArgs += "-opt-in=com.google.accompanist.insets.ExperimentalAnimatedInsets"
        freeCompilerArgs += "-opt-in=com.google.accompanist.permissions.ExperimentalPermissionsApi"
        freeCompilerArgs += "-opt-in=kotlinx.serialization.InternalSerializationApi"
        freeCompilerArgs += "-opt-in=kotlinx.serialization.ExperimentalSerializationApi"
    }
    externalNativeBuild {
        cmake {
            path(File("../common/src/commonMain/cpp/android/CMakeLists.txt"))
        }
    }
    buildTypes {
        getByName("release") {
            isMinifyEnabled = false
        }
    }
    buildFeatures {
        buildConfig = true
    }
   packaging {
        resources {
            excludes += "/META-INF/{AL2.0,LGPL2.1}"
        }
        jniLibs.useLegacyPackaging = true
    }
    android.sourceSets["main"].assets.setSrcDirs(listOf("../common/src/commonMain/resources/assets"))
    val isRelease = gradle.startParameter.taskNames.find { it.lowercase().contains("release") } != null
    val isBundle = gradle.startParameter.taskNames.find { it.lowercase().contains("bundle") } != null
    // Comma separated list of languages that will be included in the apk
    android.defaultConfig.resourceConfigurations += listOf(
        "en",
        "ar",
        "bg",
        "ca",
        "cs",
        "de",
        "es",
        "fa",
        "fi",
        "fr",
        "hu",
        "in",
        "it",
        "iw",
        "ja",
        "lt",
        "nl",
        "pl",
        "pt-rBR",
        "ro",
        "ru",
        "th",
        "tr",
        "uk",
        "vi",
        "zh-rCN"
    )
    ndkVersion = "23.1.7779620"
    if (isBundle) {
        defaultConfig.ndk.abiFilters("arm64-v8a", "armeabi-v7a")
    } else {
        splits {
            abi {
                isEnable = true
                reset()
                if (isRelease) {
                    include("arm64-v8a", "armeabi-v7a")
                } else {
                    include("arm64-v8a", "armeabi-v7a")
                    isUniversalApk = false
                }
            }
        }
    }
}

// The graph is checked rather than the requested task, because every aggregate task
// (assemble, assembleRelease, build, bundle, ...) packages these variants too.
val projectPath = project.path
val apkTasks = setOf("packageFossDebug", "packageGoogleDebug", "packageFossRelease", "packageGoogleRelease")
val apkTaskPaths = apkTasks.map { "$projectPath:$it" }.toSet()
val bundleTaskPaths = apkTaskPaths.map { it + "Bundle" }.toSet()
gradle.taskGraph.whenReady {
    if (hasTask("$projectPath:packageGoogleRelease")) {
        throw GradleException("A release apk must not include Play Billing, use assembleFossRelease or bundleGoogleRelease")
    }
    if (hasTask("$projectPath:packageFossReleaseBundle")) {
        throw GradleException("An app bundle must include Play Billing, use bundleGoogleRelease or assembleFossRelease")
    }
    // `isBundle` above is derived from the whole invocation, so a bundle in it disables abi splits
    if (apkTaskPaths.any { hasTask(it) } && bundleTaskPaths.any { hasTask(it) }) {
        throw GradleException("Build the apks and the bundle in separate invocations, the bundle disables abi splits")
    }
}

// Автоматическое скачивание AAR-библиотеки sherpa-onnx, если ее нет локально
val sherpaAarFile = file("libs/sherpa-onnx.aar")

val downloadSherpaAar by tasks.registering {
    outputs.file(sherpaAarFile)
    doLast {
        if (!sherpaAarFile.exists()) {
            sherpaAarFile.parentFile.mkdirs()
            println("--> Скачивание sherpa-onnx-1.13.8.aar из GitHub Releases...")

            val downloadUrl = URI("https://github.com/k2-fsa/sherpa-onnx/releases/download/v1.13.8/sherpa-onnx-1.13.8.aar").toURL()
            val connection = downloadUrl.openConnection() as HttpURLConnection
            connection.instanceFollowRedirects = true
            connection.connectTimeout = 30_000
            connection.readTimeout = 60_000

            connection.inputStream.use { input ->
                sherpaAarFile.outputStream().use { output ->
                    input.copyTo(output)
                }
            }
            println("--> sherpa-onnx.aar успешно загружен!")
        }
    }
}

// Запускать скачивание перед началом компиляции проекта
tasks.matching { it.name.startsWith("pre") && it.name.endsWith("Build") }.configureEach {
    dependsOn(downloadSherpaAar)
}

dependencies {
    implementation("com.google.guava:guava:31.1-android")
    implementation(project(":common"))
    "googleImplementation"("com.android.billingclient:billing:9.1.0")
    implementation("androidx.core:core-ktx:1.13.1")
    //implementation("androidx.compose.ui:ui:${rootProject.extra["compose.version"] as String}")
    //implementation("androidx.compose.material:material:$compose_version")
    //implementation("androidx.compose.ui:ui-tooling-preview:$compose_version")
    implementation("androidx.appcompat:appcompat:1.7.0")
    implementation("androidx.lifecycle:lifecycle-runtime-ktx:2.8.4")
    implementation("androidx.lifecycle:lifecycle-process:2.8.4")
    implementation("androidx.activity:activity-compose:1.9.1")
    val workVersion = "2.9.1"
    implementation("androidx.work:work-runtime-ktx:$workVersion")
    implementation("androidx.work:work-multiprocess:$workVersion")

    implementation("com.jakewharton:process-phoenix:3.0.0")

    //Camera Permission
    implementation("com.google.accompanist:accompanist-permissions:0.34.0")

    //implementation("androidx.compose.material:material-icons-extended:$compose_version")
    //implementation("androidx.compose.ui:ui-util:$compose_version")

    testImplementation("junit:junit:4.13.2")
    androidTestImplementation("androidx.test.ext:junit:1.2.1")
    androidTestImplementation("androidx.test.espresso:espresso-core:3.6.1")
    //androidTestImplementation("androidx.compose.ui:ui-test-junit4:$compose_version")
    debugImplementation("androidx.compose.ui:ui-tooling:1.6.4")

  val cameraxVersion = "1.5.0"
    implementation("androidx.camera:camera-core:$cameraxVersion")
    implementation("androidx.camera:camera-camera2:$cameraxVersion")
    implementation("androidx.camera:camera-lifecycle:$cameraxVersion")
    implementation("androidx.camera:camera-view:$cameraxVersion")

    // Модуль для записи видео (обязателен для VideoCapture и Recorder из CameraActivity)
    implementation("androidx.camera:camera-video:$cameraxVersion")

    // Модуль OEM-расширений (ночной режим, HDR, боке)
    implementation("androidx.camera:camera-extensions:$cameraxVersion")

// Движок автономного распознавания речи (Next-gen Kaldi / ONNX)
    implementation(files("libs/sherpa-onnx.aar"))
    
    // Корутины для фоновой обработки
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-android:1.8.1")

    // Material 3 с поддержкой Dynamic Colors (Monet)
    implementation("androidx.compose.material3:material3:1.3.1")
}
tasks {
    val compressApk by creating {
        doLast {
            val javaHome = System.getProperties()["java.home"] ?: org.gradle.internal.jvm.Jvm.current().javaHome
            val sdkDir = android.sdkDirectory.absolutePath
            // A single invocation can package more than one variant, for example assembleDebug
            gradle.taskGraph.allTasks.filter { it.path in apkTaskPaths }.forEach { packageTask ->
                val variant = packageTask.name.removePrefix("package")
                val buildType: String = if (variant.endsWith("Release")) "release" else "debug"
                val keyAlias: String
                val keyPassword: String
                val storeFile: String
                val storePassword: String
                if (project.properties["android.injected.signing.key.alias"] != null) {
                    keyAlias = project.properties["android.injected.signing.key.alias"] as String
                    keyPassword = project.properties["android.injected.signing.key.password"] as String
                    storeFile = project.properties["android.injected.signing.store.file"] as String
                    storePassword = project.properties["android.injected.signing.store.password"] as String
                } else {
                    try {
                        val gradleConfig = android.signingConfigs.getByName(buildType)
                        keyAlias = gradleConfig.keyAlias!!
                        keyPassword = gradleConfig.keyPassword!!
                        storeFile = gradleConfig.storeFile!!.absolutePath
                        storePassword = gradleConfig.storePassword!!
                    } catch (e: UnknownDomainObjectException) {
                        // There is no signing config for current build type, can"t sign the apk
                        println("No signing configs for this build type: $buildType")
                        return@forEach
                    }
                }
                val outputDir = packageTask.outputs.files.files.last()
                exec {
                    workingDir("../../scripts/android")
                    environment = mapOf(
                      "JAVA_HOME" to "$javaHome",
                      "PATH" to "${System.getenv("PATH")}:$javaHome/bin"
                    )
                    commandLine = listOf(
                        "./compress-and-sign-apk.sh",
                        "${rootProject.extra["compression.level"]}",
                        "$outputDir",
                        sdkDir,
                        storeFile,
                        storePassword,
                        keyAlias,
                        keyPassword
                    )
                }

                if (project.properties["android.injected.signing.key.alias"] != null && buildType == "release") {
                    val flavor = variant.removeSuffix("Release").lowercase()
                    mapOf("arm64-v8a" to "simplex.apk", "armeabi-v7a" to "simplex-armv7a.apk").forEach { (abi, name) ->
                        if (!File(outputDir, "android-$flavor-$abi-release.apk").renameTo(File(outputDir, name))) {
                            logger.warn("No $abi apk to rename to $name")
                        }
                    }
                }
            }
            // View all gradle properties set
            // project.properties.each { k, v -> println "$k -> $v" }
        }
    }

    // Don"t do anything if no compression is needed
    if (rootProject.extra["compression.level"] as Int != 0) {
        whenTaskAdded {
            if (name in apkTasks) {
                finalizedBy(compressApk)
            }
        }
    }
}
