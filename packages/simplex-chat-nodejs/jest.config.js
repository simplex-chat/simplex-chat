module.exports = {
  preset: "ts-jest",
  globalSetup: "./tests/setup.ts",
  maxWorkers: 1,
  testEnvironment: "node",
  transform: {
    '^.+\\.ts$': ['ts-jest', {
      tsconfig: 'tests/tsconfig.json'
    }]
  }
}
