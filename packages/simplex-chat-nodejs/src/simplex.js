const {install, installAddon} = require("../src/download-libs")

let loaded

function load() {
  loaded ??= Promise.all([install(), installAddon()]).then(([libPath, addonPath]) => {
    const addon = {exports: {}}
    process.dlopen(addon, addonPath)
    addon.exports.load(libPath)
    return addon.exports
  }).catch((e) => {
    loaded = undefined
    throw e
  })
  return loaded
}

for (const name of ["chat_migrate_init", "chat_migrate_init_queue", "chat_close_store", "chat_send_cmd", "chat_recv_msg_wait", "chat_write_file", "chat_read_file", "chat_encrypt_file", "chat_decrypt_file"]) {
  exports[name] = async (...args) => (await load())[name](...args)
}
