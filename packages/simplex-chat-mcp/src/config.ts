import {Mode} from "./actions"
import {maxNameLength, validName} from "./messages"

// spec: spec/architecture.md#config-config
export interface Config {
  mainGroup: string
  approve: Mode
  expireHours: number
}

type Key = keyof Config

// spec: spec/architecture.md#config-config
export const defaultConfig: Config = {mainGroup: "Claude Control", approve: "requests", expireHours: 23}

// spec: spec/architecture.md#config-config
export const configVariables: {[key in Key]: string} = {
  mainGroup: "CLAUDE_PLUGIN_OPTION_MAIN_GROUP",
  approve: "CLAUDE_PLUGIN_OPTION_APPROVE",
  expireHours: "CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS",
}

const optionNames: {[key in Key]: string} = {mainGroup: "main_group", approve: "approve", expireHours: "expire_hours"}
const configurePath = "/plugin > Installed > simplex > Configure options"
const machineVariable = "SIMPLEX_MCP_MACHINE"
// spec: spec/api.md#plugin-tools
const unattendedEntrypoints = ["sdk-cli", "sdk-ts", "sdk-py"]

export class ConfigError extends Error {}

// spec: spec/architecture.md#config-interactiveentrypoint
export function interactiveEntrypoint(env: NodeJS.ProcessEnv = process.env): boolean {
  return !unattendedEntrypoints.includes(env.CLAUDE_CODE_ENTRYPOINT ?? "")
}

// spec: spec/architecture.md#config-interactiveentrypoint
export function interactive(env: NodeJS.ProcessEnv = process.env): boolean {
  return env.CLAUDE_CODE_SESSION_ATTENDED !== "0" && interactiveEntrypoint(env)
}

// spec: spec/architecture.md#config-readconfig
export function readConfig(env: NodeJS.ProcessEnv = process.env): Config {
  const value = (key: Key): string | undefined => {
    const given = env[configVariables[key]]?.trim()
    return given === undefined || given === "" ? undefined : given
  }
  return {mainGroup: withMachine(mainGroup(value("mainGroup")), env[machineVariable]?.trim() || undefined), approve: approve(value("approve")), expireHours: expireHours(value("expireHours"))}
}

// spec: spec/api.md#plugin-options
function mainGroup(value: string | undefined): string {
  if (value === undefined) return defaultConfig.mainGroup
  const valid = validName(value)
  if (valid !== value) throw configError("mainGroup", value, nameExpected(valid))
  return value
}

// spec: spec/architecture.md#config-withmachine
function withMachine(name: string, machine: string | undefined): string {
  if (machine === undefined) return name
  const valid = validName(machine)
  if (valid !== machine) throw new ConfigError(`${machineVariable}: ${nameExpected(valid)} expected, got ${machine}. Change it in the environment of this machine.`)
  const named = `${name} - ${machine}`
  if (validName(named) !== named) throw new ConfigError(`Plugin option ${optionNames.mainGroup} with ${machineVariable}: a valid SimpleX name of at most ${maxNameLength} characters expected, got ${named}. Change one of them.`)
  return named
}

function nameExpected(valid: string): string {
  return valid === "" ? "a valid SimpleX name" : `a valid SimpleX name, such as ${valid}`
}

// spec: spec/api.md#plugin-options
function approve(value: string | undefined): Mode {
  if (value === undefined) return defaultConfig.approve
  if (value !== "requests" && value !== "host") throw configError("approve", value, "requests or host")
  return value
}

// spec: spec/api.md#plugin-options
function expireHours(value: string | undefined): number {
  if (value === undefined) return defaultConfig.expireHours
  const hours = Number(value)
  if (!/^\d+$/.test(value) || hours < 1 || hours > 23) throw configError("expireHours", value, "hours from 1 to 23")
  return hours
}

function configError(key: Key, value: string, expected: string): ConfigError {
  return new ConfigError(`Plugin option ${optionNames[key]}: ${expected} expected, got ${value}. Change it in ${configurePath}.`)
}
