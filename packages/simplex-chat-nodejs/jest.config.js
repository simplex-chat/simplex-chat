const path = require("path")

// Tests load the locally built add-on instead of downloading the released one.
process.env.SIMPLEX_ADDON_PATH ??= path.join(__dirname, "build", "Release", "simplex.node")

module.exports = {
  preset: "ts-jest",
  maxWorkers: 1,
  testEnvironment: "node",
  transform: {
    '^.+\\.ts$': ['ts-jest', {
      tsconfig: 'tests/tsconfig.json'
    }]
  }
}
