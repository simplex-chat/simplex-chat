import {Mode} from "./actions"
import {validName} from "./messages"

export interface Config {
  mainGroup: string
  approve: Mode
  expireHours: number
}

type Key = keyof Config

export const defaultConfig: Config = {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}

export const configVariables: {[key in Key]: string} = {
  mainGroup: "CLAUDE_PLUGIN_OPTION_MAIN_GROUP",
  approve: "CLAUDE_PLUGIN_OPTION_APPROVE",
  expireHours: "CLAUDE_PLUGIN_OPTION_EXPIRE_HOURS",
}

const optionNames: {[key in Key]: string} = {mainGroup: "main_group", approve: "approve", expireHours: "expire_hours"}
const configurePath = "/plugin > Installed > simplex > Configure options"

export class ConfigError extends Error {}

export function readConfig(env: NodeJS.ProcessEnv = process.env): Config {
  const value = (key: Key): string | undefined => {
    const given = env[configVariables[key]]?.trim()
    return given === undefined || given === "" ? undefined : given
  }
  return {mainGroup: mainGroup(value("mainGroup")), approve: approve(value("approve")), expireHours: expireHours(value("expireHours"))}
}

function mainGroup(value: string | undefined): string {
  if (value === undefined) return defaultConfig.mainGroup
  const valid = validName(value)
  if (valid !== value) throw configError("mainGroup", value, valid === "" ? "a valid SimpleX name" : `a valid SimpleX name, such as ${valid}`)
  return value
}

function approve(value: string | undefined): Mode {
  if (value === undefined) return defaultConfig.approve
  if (value !== "requests" && value !== "host") throw configError("approve", value, "requests or host")
  return value
}

function expireHours(value: string | undefined): number {
  if (value === undefined) return defaultConfig.expireHours
  const hours = Number(value)
  if (!/^\d+$/.test(value) || hours < 1 || hours > 23) throw configError("expireHours", value, "hours from 1 to 23")
  return hours
}

function configError(key: Key, value: string, expected: string): ConfigError {
  return new ConfigError(`Plugin option ${optionNames[key]}: ${expected} expected, got ${value}. Change it in ${configurePath}.`)
}
