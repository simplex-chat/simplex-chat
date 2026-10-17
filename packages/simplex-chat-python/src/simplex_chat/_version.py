"""Package version and the simplex-chat-libs release it loads.

Written by scripts/ci/packages_release.py: the release PR bumps the version
and pins new library releases with the SHA-256 of their assets.
"""

__version__ = "7.1.0b6.post1"  # PEP 440 — read by hatchling for wheel metadata
LIBS_RELEASE = "core-7.1.0.11"  # simplex-chat-libs release tag
LIBS_SHA256 = {
    "simplex-chat-libs-linux-aarch64.zip": "d66aa1b30b9fa20225dad8ef75a9c605961e1c0adde2ba0283e2fcac4e15d260",
    "simplex-chat-libs-linux-x86_64-postgres.zip": "925b3166f969b12082ff8ee659e5f2a9933fe40120deb80c9aa3002a5ba6c803",
    "simplex-chat-libs-linux-x86_64.zip": "dabad0824e8813ea4a084fbf37e1ba4cf6dcea9cd8ccadc521fec200f246208a",
    "simplex-chat-libs-macos-aarch64.zip": "aa74608b149fff7508a47eaac4284dcc6d189c6ed8b7c18428df9158bc176a61",
    "simplex-chat-libs-macos-x86_64.zip": "a0e21457e1077be7f71b46c1c27452945b2cee2b6c50efb0004412ce5b7042ec",
    "simplex-chat-libs-windows-x86_64.zip": "c01dad574daa101a1cbfd31ddbf2b3d0d986a4a42f97d789d2f878b2050b2b4b",
}
