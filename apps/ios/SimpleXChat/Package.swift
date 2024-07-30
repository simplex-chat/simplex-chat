// swift-tools-version: 5.10
import PackageDescription

let package = Package(
    name: "SimpleXChat",
    platforms: [.iOS(.v15)],
    products: [
        .library(name: "SimpleXChat", type: .dynamic, targets: ["Core"]),
    ],
    dependencies: [
        .package(url: "https://github.com/jpsim/Yams.git", exact: "5.1.2"),
        .package(url: "https://github.com/kirualex/SwiftyGif.git", revision: "5e8619335d394901379c9add5c4c1c2f420b3800"),
    ],
    targets: [
        .target(
            name: "Core",
            dependencies: ["Yams", "SwiftyGif", "External"]
        ),
        .target(
            name: "External",
            publicHeadersPath: ".",
            linkerSettings: [
                .linkedLibrary("ffi"),
                .linkedLibrary("gmp"),
                .linkedLibrary("gmpxx"),
                .linkedLibrary("HSsimplex-chat-6.0.0.3-5yF58NJ20MV4vZ7jQKFAxB-ghc9.6.3"),
                .linkedLibrary("HSsimplex-chat-6.0.0.3-5yF58NJ20MV4vZ7jQKFAxB"),
                .unsafeFlags(["-L../Libraries"])
            ]
        ),
    ]
)
