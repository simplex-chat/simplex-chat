import {defineConfig} from "vitest/config"
import path from "path"

export default defineConfig({
  test: {
    globals: true,
    testTimeout: 10000,
    // Clear backend signals — .npmrc next to package.json otherwise injects
    // npm_config_simplex_backend into every test's env, breaking sqlite-default
    // assumptions in parseConfig tests.
    env: {
      SIMPLEX_BACKEND: "",
      npm_config_simplex_backend: "",
    },
  },
  resolve: {
    alias: {
      "simplex-chat": path.resolve(__dirname, "test/__mocks__/simplex-chat.js"),
      "@simplex-chat/types": path.resolve(__dirname, "test/__mocks__/simplex-chat-types.js"),
    },
  },
})
