// swift-tools-version: 6.0

import PackageDescription

let package = Package(
    name: "SimpleXNative",
    platforms: [.macOS(.v14)],
    products: [
        .executable(name: "SimpleXNative", targets: ["SimpleXNative"]),
    ],
    targets: [
        .target(
            name: "CoreBridge",
            publicHeadersPath: "include",
            linkerSettings: [.linkedLibrary("dl")]
        ),
        .executableTarget(
            name: "SimpleXNative",
            dependencies: ["CoreBridge"]
        ),
        .testTarget(
            name: "SimpleXNativeTests",
            dependencies: ["SimpleXNative", "CoreBridge"]
        ),
    ],
    swiftLanguageModes: [.v5]
)
