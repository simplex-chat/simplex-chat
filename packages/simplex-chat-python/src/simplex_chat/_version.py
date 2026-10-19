"""Package version and the simplex-chat-libs release it loads.

Written by scripts/ci/packages_release.py prepare: it bumps the version
and pins new library releases with the SHA-256 of their assets.
"""

__version__ = "7.1.0b6.post1"  # PEP 440 — read by hatchling for wheel metadata
LIBS_RELEASE = "core-7.1.0.12"  # simplex-chat-libs release tag
LIBS_SHA256 = {
    "simplex-chat-libs-linux-aarch64.zip": "924b8e14eaba5e309fa584c4bfe598421cf64983d63790c1c45247f592f60055",
    "simplex-chat-libs-linux-x86_64-postgres.zip": "323f908b45dde751d6082f8885d07d81fba8a5ab8b2df7d3b8ab3a87e56299e5",
    "simplex-chat-libs-linux-x86_64.zip": "995409e963db408bcf896361b669944484f845b6744fefe479aebb0ca2e7c5cd",
    "simplex-chat-libs-macos-aarch64.zip": "64464aa48b996e5a96b4aa651bc57bc69e412d3c3804b589c67bd9dc52faddbf",
    "simplex-chat-libs-macos-x86_64.zip": "94664f96ff7a2cd5a419096e7e843928faa31cb3addb8869fa1e4b5a0d0b3629",
    "simplex-chat-libs-windows-x86_64.zip": "2e10844e5c119921139c373ee2971341aceef7dc4198429d3be6b1570a009d15",
}
