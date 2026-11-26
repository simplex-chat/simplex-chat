# Preserve the manifest order for the final mobile library linker flags.
builtins.filter (line: builtins.isString line && line != "")
  (builtins.split "\r?\n" (builtins.readFile ./chat-exports.txt))
