const path = require("path")

// Tests load the locally built add-on instead of downloading the released one.
process.env.SIMPLEX_ADDON_PATH ??= path.join(__dirname, "build", "Release", "simplex.node")

module.exports = {
  preset: "ts-jest",
  maxWorkers: 1,
  // a test with a new database runs every migration, each committed with a disk flush: seconds on slow CI runners
  testTimeout: 30000,
  testEnvironment: "node",
  transform: {
    '^.+\\.ts$': ['ts-jest', {
      tsconfig: 'tests/tsconfig.json'
    }]
  }
}
