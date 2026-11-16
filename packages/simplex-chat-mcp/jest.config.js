module.exports = {
  preset: "ts-jest",
  maxWorkers: 1,
  testEnvironment: "node",
  testTimeout: 30000,
  transform: {
    "^.+\\.ts$": ["ts-jest", {tsconfig: "test/tsconfig.json", diagnostics: {ignoreCodes: [151002]}}]
  }
}
