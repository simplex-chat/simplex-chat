"""Package version and the simplex-chat-libs release it loads.

Written by scripts/ci/packages_release.py prepare: it bumps the version
and pins new library releases with the SHA-256 of their assets.
"""

__version__ = "7.1.0b6.post2"  # PEP 440 — read by hatchling for wheel metadata
LIBS_RELEASE = "core-7.1.0.14"  # simplex-chat-libs release tag
LIBS_SHA256 = {
    "simplex-chat-libs-linux-aarch64.zip": "375a6021f0c2e5d79b2cd192e17c6c22c8eae73f46e3758fba24ee5c26a671aa",
    "simplex-chat-libs-linux-x86_64-postgres.zip": "c3e8a54d37801e685c6361c4e7e53e1f5163d3aa9195dcfe686b6cf90e668ef3",
    "simplex-chat-libs-linux-x86_64.zip": "cc41ab9049d61be3b386d799fa9394f2870e8ae3b2b9b8745dd3f53ba99f211e",
    "simplex-chat-libs-macos-aarch64.zip": "ac3a46004f25976f71b3ead8a20b9a01c170fe12f125a0a6799ae97ea990f234",
    "simplex-chat-libs-macos-x86_64.zip": "0d85ffcccc3a6dfe3e0da18d0b7be9ef36ad27381f047ef66cfde67ee4a7e882",
    "simplex-chat-libs-windows-x86_64.zip": "f1d1c7efc333a5ec5328a5843c9d3c9ef038fb2886c0b60ac12e87a5c1e85bbd",
}
