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
