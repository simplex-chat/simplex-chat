# App tests on GitHub's runners

Two workflows build the apps from a branch's own code and test them, only when asked: `ios-sim` (the iOS app in a simulator on a macOS runner) and `desktop-linux` (the desktop app on a Linux runner). Each runs a scripted smoke test and, on request, a Claude session that follows a test plan, and a test backend (relay, directory, names server, peers). Details: [iOS](ios-sim/README.md), [desktop](desktop-linux/README.md).

## Starting a run

Pushes start nothing: a run is started by hand, on any branch, with inputs that are off by default and combine.

| Input | iOS | Desktop |
|---|---|---|
| none | scripted test | scripted test |
| `claude` | scripted test, then Claude | scripted test, then Claude |
| `backend` | also the test backend | also the test backend |
| `release` | — | also the latest release's onboarding, to tell a branch's failure from the runner's |

In the Actions tab: `ios-sim` or `desktop-linux` → Run workflow, choose the branch and tick the inputs. With the GitHub CLI, each input is a field:

```sh
gh workflow run ios-sim.yml --ref <branch>
gh workflow run ios-sim.yml --ref <branch> -f claude=true
gh workflow run desktop-linux.yml --ref <branch> -f claude=true -f backend=true -f release=true
```

A newer run of a workflow on the same branch cancels the older one.

## How long a run takes

| Run | iOS | Desktop |
|---|---|---|
| Scripted test, builds reused | about 25 min, plus the wait for a macOS runner, which the team's builds share (minutes to an hour) | about 10 min |
| The core rebuilt, after Haskell changes | 30–45 min more | about 25 min more |
| With the test backend | 20–45 min more when it is rebuilt, then about 10 min to start | about 30 min more when it is rebuilt, then a few minutes to start |
| With Claude on the smoke plan | 10–15 min more | about 4 min more |

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. The environment's branch rule alone decides which branches may use it: on a branch it doesn't allow, a run that asks for Claude fails at the Claude job's environment check, and the scripted test runs as usual. Each run spends the subscription's usage limits; the job summary gives its tokens and cost estimated at API prices.
