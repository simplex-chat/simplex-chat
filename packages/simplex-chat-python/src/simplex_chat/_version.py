"""Package version and the simplex-chat-libs release it loads.

Written by scripts/ci/packages_release.py prepare: it bumps the version
and pins new library releases with the SHA-256 of their assets.
"""

__version__ = "7.1.3"  # PEP 440 — read by hatchling for wheel metadata
LIBS_RELEASE = "core-7.1.3.0"  # simplex-chat-libs release tag
LIBS_SHA256 = {
    "simplex-chat-libs-linux-aarch64.zip": "46e296417de8cddc0d6f800d7c634dec410a1af20ea38ff31f224ef388aaeb00",
    "simplex-chat-libs-linux-x86_64-postgres.zip": "ea89b4e95836c79c887cc13a2dc9fc66e319b21ee4148ca38817539657601050",
    "simplex-chat-libs-linux-x86_64.zip": "12b6ea97c64a5ea53a66f47e827dc8c0d6bc767fd44b22aab47786ac90e91d01",
    "simplex-chat-libs-macos-aarch64.zip": "b7612b05cd17267f4e24efacfd16fa4a9f9b0c39b34c2f52465e34395492a452",
    "simplex-chat-libs-macos-x86_64.zip": "ce7a9a2be0cd0e6782902d8ce4420e8660e4e2360d7fc0248584a0163f1a236d",
    "simplex-chat-libs-windows-x86_64.zip": "a9ed55c82753d57cfddaddee78cc989b0edf3fe5fe08d7214e6f84ab43609999",
}
