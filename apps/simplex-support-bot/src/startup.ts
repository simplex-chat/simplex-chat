import {existsSync} from "fs"
import {execSync} from "child_process"
import {log, logError} from "./util.js"

// Resolve display_names table conflicts before bot.run updates the profile.
// The SimpleX Chat store enforces unique (user_id, local_display_name) in display_names.
// If the desired name is already used by a contact or group, the profile update fails
// with duplicateName. This renames the conflicting entry to free up the name.
export function resolveDisplayNameConflict(dbPrefix: string, desiredName: string): void {
  const dbFile = `${dbPrefix}_chat.db`
  if (!existsSync(dbFile)) return
  const esc = desiredName.replace(/'/g, "''")
  try {
    // If user already has this display name, no conflict — Haskell takes the no-change branch
    const isUserName = execSync(
      `sqlite3 "${dbFile}" "SELECT COUNT(*) FROM users WHERE local_display_name = '${esc}'"`,
      {encoding: "utf-8"}
    ).trim()
    if (isUserName !== "0") return

    // Check if the name exists in display_names at all
    const count = execSync(
      `sqlite3 "${dbFile}" "SELECT COUNT(*) FROM display_names WHERE local_display_name = '${esc}'"`,
      {encoding: "utf-8"}
    ).trim()
    if (count === "0") return

    // Rename the conflicting entry (contact/group) to free the name
    const newName = `${esc}_1`
    log(`Display name conflict: "${desiredName}" already in display_names, renaming to "${newName}"`)
    const sql = [
      `UPDATE contacts SET local_display_name = '${newName}' WHERE local_display_name = '${esc}';`,
      `UPDATE groups SET local_display_name = '${newName}' WHERE local_display_name = '${esc}';`,
      `UPDATE display_names SET local_display_name = '${newName}', ldn_suffix = 1 WHERE local_display_name = '${esc}';`,
    ].join(" ")
    execSync(`sqlite3 "${dbFile}" "${sql}"`, {encoding: "utf-8"})
    log("Display name conflict resolved")
  } catch (err) {
    logError("Failed to resolve display name conflict (sqlite3 may not be available)", err)
  }
}
