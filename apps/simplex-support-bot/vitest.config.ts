import {defineConfig} from "vitest/config"

export default defineConfig({
  test: {
    include: ["bot.test.ts"],
    typecheck: {
      include: ["bot.test.ts"],
    },
  },
})
