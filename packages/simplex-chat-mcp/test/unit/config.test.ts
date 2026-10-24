import {ConfigError, configVariables, defaultConfig, readConfig} from "../../src/config"

const env = (values: {mainGroup?: string; approve?: string; expireHours?: string}): NodeJS.ProcessEnv =>
  Object.fromEntries(Object.entries(values).map(([key, value]) => [configVariables[key as keyof typeof configVariables], value]))

describe("plugin config", () => {
  it("has defaults for missing and empty options", () => {
    expect(readConfig({})).toEqual(defaultConfig)
    expect(readConfig(env({mainGroup: "", approve: "  ", expireHours: ""}))).toEqual({mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23})
  })

  it("reads every option", () => {
    expect(readConfig(env({mainGroup: "My Agents", approve: "host", expireHours: "4"}))).toEqual({mainGroup: "My Agents", approve: "host", expireHours: 4})
  })

  it("names the option, the value and where to change it", () => {
    const where = "Change it in /plugin > Installed > simplex > Configure options."
    expect(() => readConfig(env({approve: "always"}))).toThrow(new ConfigError(`Plugin option approve: requests or host expected, got always. ${where}`))
    expect(() => readConfig(env({expireHours: "24"}))).toThrow(new ConfigError(`Plugin option expire_hours: hours from 1 to 23 expected, got 24. ${where}`))
    expect(() => readConfig(env({expireHours: "1.5"}))).toThrow(new ConfigError(`Plugin option expire_hours: hours from 1 to 23 expected, got 1.5. ${where}`))
    expect(() => readConfig(env({expireHours: "0"}))).toThrow(ConfigError)
  })

  it("refuses a Main group name that is not a valid SimpleX name, with the valid form", () => {
    expect(() => readConfig(env({mainGroup: "#agents"}))).toThrow(
      new ConfigError("Plugin option main_group: a valid SimpleX name, such as agents expected, got #agents. Change it in /plugin > Installed > simplex > Configure options."),
    )
    expect(() => readConfig(env({mainGroup: "..."}))).toThrow(
      new ConfigError("Plugin option main_group: a valid SimpleX name expected, got .... Change it in /plugin > Installed > simplex > Configure options."),
    )
  })
})
