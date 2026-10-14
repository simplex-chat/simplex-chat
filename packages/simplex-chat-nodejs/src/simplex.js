// The add-on is downloaded from the libsimplex release, so it is loaded at runtime by core.loadLibrary.
const NOT_LOADED = "libsimplex is not loaded, call core.loadLibrary(backend) first"
const BINDINGS = ["chat_migrate_init", "chat_migrate_init_queue", "chat_close_store", "chat_send_cmd", "chat_recv_msg_wait",
  "chat_write_file", "chat_read_file", "chat_encrypt_file", "chat_decrypt_file"]

let addon

exports.load = (addonPath, libPath) => {
  if (!addon) {
    const addonModule = {exports: {}}
    process.dlopen(addonModule, addonPath)
    addon = addonModule.exports
  }
  addon.load(libPath)
}

for (const name of BINDINGS) {
  exports[name] = (...args) => {
    if (!addon) throw new Error(NOT_LOADED)
    return addon[name](...args)
  }
}
