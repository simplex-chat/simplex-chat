#!/bin/bash

# Replaces the database of the app that smoke-test.sh left running with the encrypted database that the previous
# release's CLI made (scripts/simplex-chat-build.py test old-database), enters its key on the passphrase screen and
# waits for the chat list with its group, so the app's core and OpenSSL open and migrate that database.
# Usage: scripts/ci/ios-sim/old-database.sh <old database dir>
# Needs SIM_UDID and UI_OUT from smoke-test.sh.

set -euo pipefail

old=$1
bundle_id=chat.simplex.app
ui=$(dirname "$0")/ui
key="old-release-key"
group=team

# the snapshot tests after the smoke test may have stopped the app already
xcrun simctl terminate "$SIM_UDID" "$bundle_id" || true
container=$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" group.chat.simplex.app)
data=$(xcrun simctl get_app_container "$SIM_UDID" "$bundle_id" data)
trap '"$ui" shot old-database-failed || true' ERR
echo "::group::database files before the swap"
ls -la "$container" "$data/Documents" || true
echo "::endgroup::"
rm -f "$container"/simplex_v1_chat.db* "$container"/simplex_v1_agent.db*
cp "$old/simplex_chat.db" "$container/simplex_v1_chat.db"
cp "$old/simplex_agent.db" "$container/simplex_v1_agent.db"

# iOS otherwise asks for the camera over the passphrase screen, hiding its button from the accessibility tree
xcrun simctl privacy "$SIM_UDID" grant camera "$bundle_id"
# the keychain still has the key of the smoke test's database, so the app asks for the right one
xcrun simctl launch "$SIM_UDID" "$bundle_id"
"$ui" wait "Save passphrase and open chat" 180
"$ui" shot old-database-passphrase
"$ui" tap-value "Enter passphrase…"
"$ui" type "$key"
"$ui" tap "Save passphrase and open chat"

"$ui" wait "$group" 180
"$ui" shot old-database-chat-list
echo "the app opened the previous release's database and shows its group $group"
