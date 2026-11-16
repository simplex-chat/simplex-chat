import {defineConfig} from "vitest/config"
import path from "path"

export default defineConfig({
  test: {
    globals: true,
    testTimeout: 10000,
  },
  resolve: {
    alias: {
      "simplex-chat": path.resolve(__dirname, "test/__mocks__/simplex-chat.js"),
      "@simplex-chat/types": path.resolve(__dirname, "test/__mocks__/simplex-chat-types.js"),
    },
  },
})
