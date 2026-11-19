@file:Suppress("UnstableApiUsage")

import org.jetbrains.kotlin.gradle.dsl.JvmTarget

plugins {
    id("com.android.application")
    id("org.jetbrains.compose")
    kotlin("android")
    id("org.jetbrains.kotlin.plugin.serialization")
    id("org.jetbrains.kotlin.plugin.compose")
}

kotlin {
    compilerOptions {
        jvmTarget.set(JvmTarget.fromTarget(providers.gradleProperty("kotlin.jvm.target").get()))
        optIn.addAll(
            "kotlinx.coroutines.DelicateCoroutinesApi",
            "androidx.compose.foundation.ExperimentalFoundationApi",
            "androidx.compose.ui.text.ExperimentalTextApi",
            "androidx.compose.material.ExperimentalMaterialApi",
            "com.google.accompanist.insets.ExperimentalAnimatedInsets",
            "com.google.accompanist.permissions.ExperimentalPermissionsApi",
            "kotlinx.serialization.InternalSerializationApi",
            "kotlinx.serialization.ExperimentalSerializationApi",
        )
    }
}

android {
    compileSdk = 37
    compileOptions {
        sourceCompatibility = JavaVersion.toVersion(providers.gradleProperty("kotlin.jvm.target").get())
        targetCompatibility = JavaVersion.toVersion(providers.gradleProperty("kotlin.jvm.target").get())
    }

    defaultConfig {
        applicationId = "chat.simplex.app"
        namespace = "chat.simplex.app"
        minSdk = 26
        targetSdk = 36
        // !!!
        // skip version code after release to F-Droid, as it uses two version codes
        versionCode = (project.extra["android.version_code"] as String).toInt()
        versionName = project.extra["android.version_name"] as String

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
        jniLibs.useLegacyPackaging = rootProject.extra["compression.level"] as Int != 0
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

dependencies {
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
}

tasks {
    val compressApk by registering {
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
                if (project.findProperty("android.injected.signing.key.alias") != null) {
                    keyAlias = project.findProperty("android.injected.signing.key.alias") as String
                    keyPassword = project.findProperty("android.injected.signing.key.password") as String
                    storeFile = project.findProperty("android.injected.signing.store.file") as String
                    storePassword = project.findProperty("android.injected.signing.store.password") as String
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
                providers.exec {
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
                }.result.get()

                if (project.findProperty("android.injected.signing.key.alias") != null && buildType == "release") {
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
