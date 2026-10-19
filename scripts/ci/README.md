# App tests on GitHub's runners

Two workflows build the apps from a branch's own code and test them, only when asked: `ios-sim` (the iOS app in a simulator on a macOS runner) and `desktop-linux` (the desktop app on a Linux runner). Each runs a scripted smoke test and, on request, a Claude session that follows a test plan, and a test backend (relay, directory, names server, peers). Details: [iOS](ios-sim/README.md), [desktop](desktop-linux/README.md).

## Run tags

Put a tag in the message of the commit you push to a branch named `f/…`. Tags combine: each workflow runs everything its tags ask for.

| Tag | iOS | Desktop |
|---|---|---|
| `[app-ci]` | scripted test | scripted test |
| `[app-ci:ios]` | scripted test | — |
| `[app-ci:desktop]` | — | scripted test |
| `[claude-test]` | scripted test, then Claude | scripted test, then Claude |
| `[claude-test:ios]` | scripted test, then Claude | — |
| `[claude-test:desktop]` | — | scripted test, then Claude |
| `[ci-backend]`, with one of the above | also the test backend | also the test backend |
| `[ci-release]`, with one of the above | — | also the latest release's onboarding, to tell a branch's failure from the runner's |

Only the pushed head commit counts, and a tag counts anywhere in its message, body included: never write one in a message unless that push should run it. A push that asks for nothing starts no job and doesn't cancel a running test; a newer requested run on the same branch cancels the older one. Tags start runs only on `f/…` branches, so pull requests from other branches don't list skipped runs; on any other branch, master and stable included, run a workflow by hand.

## Running a workflow by hand

Actions → `ios-sim` or `desktop-linux` → Run workflow, on any branch, with checkboxes that are off by default: `claude` (the Claude session), `backend` (the test backend) and, on desktop, `release` (the latest release's onboarding).

## How long a run takes

| Run | iOS | Desktop |
|---|---|---|
| Scripted test, builds reused | about 25 min, plus the wait for a macOS runner, which the team's builds share (minutes to an hour) | about 10 min |
| The core rebuilt, after Haskell changes | 30–45 min more | about 25 min more |
| With the test backend | 20–45 min more when it is rebuilt, then about 10 min to start | about 30 min more when it is rebuilt, then a few minutes to start |
| With Claude on the smoke plan | 10–15 min more | about 4 min more |

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. The environment's branch rule alone decides which branches may use it: on a branch it doesn't allow, a run that asks for Claude fails at the Claude job's environment check, and the scripted test runs as usual. Each run spends the subscription's usage limits; the job summary gives its tokens and cost estimated at API prices.
