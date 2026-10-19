// Written by scripts/ci/packages_release.py from the simplex-chat-libs releases.

export const LIBS_RELEASE = "core-7.1.0.12"

export const ADDON_RELEASE: string | undefined = "nodejs-addon-308c0960c707"

export const SHA256: {readonly [asset: string]: string} = {
  "simplex-chat-libs-linux-aarch64.zip": "924b8e14eaba5e309fa584c4bfe598421cf64983d63790c1c45247f592f60055",
  "simplex-chat-libs-linux-x86_64-postgres.zip": "323f908b45dde751d6082f8885d07d81fba8a5ab8b2df7d3b8ab3a87e56299e5",
  "simplex-chat-libs-linux-x86_64.zip": "995409e963db408bcf896361b669944484f845b6744fefe479aebb0ca2e7c5cd",
  "simplex-chat-libs-macos-aarch64.zip": "64464aa48b996e5a96b4aa651bc57bc69e412d3c3804b589c67bd9dc52faddbf",
  "simplex-chat-libs-macos-x86_64.zip": "94664f96ff7a2cd5a419096e7e843928faa31cb3addb8869fa1e4b5a0d0b3629",
  "simplex-chat-libs-windows-x86_64.zip": "2e10844e5c119921139c373ee2971341aceef7dc4198429d3be6b1570a009d15",
  "simplex-chat-nodejs-linux-aarch64.node": "3636a82c56c48229e0326fa03d6ceb5c16dd3afbcc1e86337783dc53a7328d67",
  "simplex-chat-nodejs-linux-x86_64.node": "55f3479b03fdb57591179ed730da744f1aa362e7529df55221b60028415746e9",
  "simplex-chat-nodejs-macos-aarch64.node": "a3472872fb0816f435f52ee5471088e6545ebffb95e89fbf8e1ab82275f2d56e",
  "simplex-chat-nodejs-macos-x86_64.node": "290901f7f227019a8cee3acc832541c930c4926fc08110df25169810a9002f5a",
  "simplex-chat-nodejs-windows-x86_64.node": "ec689373b4f0fef73253a8328f60b8e902a45ea67e756d42a1f759b44dfd7339",
}
