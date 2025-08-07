const addon = require('../build/Release/addon');
const fs = require('fs');

const ctrlAndRes = addon.chat_migrate_init("db", "a", "yesUp")
const ctrl = Number(ctrlAndRes.split("\n")[0])
const res = ctrlAndRes.split("\n")[1]
console.log("Migrate ctrl:", ctrl, "res:", res)
console.log(addon.chat_send_cmd(ctrl, "/v"))
// const wait = 15_000_000
// console.log(ctrl, addon.chat_recv_msg_wait(ctrl, wait))

const path = "/tmp/write_file.txt"
const data = [0, 1, 2]
console.log(addon.chat_write_file(ctrl, path, new ArrayBuffer(data)))


fs.writeFileSync("/tmp/file_unencrypted.txt", "unencrypted")
const encRes = JSON.parse(addon.chat_encrypt_file(ctrl, "/tmp/file_unencrypted.txt", "/tmp/file_encrypted.txt"))
const key = encRes.cryptoArgs.fileKey
const nonce = encRes.cryptoArgs.fileNonce
console.log(encRes)
console.log(addon.chat_decrypt_file("/tmp/file_encrypted.txt", key, nonce, "/tmp/file_decrypted.txt"))
