# App tests from a fork: instructions for agents

For an agent that tests the iOS, desktop or Android app with the app test workflows ([CI on GitHub's runners](README.md#starting-a-run)) in its user's fork of simplex-chat. The user tells you the fork (`<owner>/simplex-chat`), its git remote (`<remote>`), and whether you may start Claude runs, which spend their Claude subscription's usage limits. Your `gh` login needs write access to the fork. Pass `--repo <owner>/simplex-chat` to every `gh` command: with simplex-chat as another remote, `gh` can pick it.

## Starting a run

1. The branch needs master's app test workflows from 9a1d3e4969 (2026-10-20) or later: after fetching simplex-chat's master, check with `git merge-base --is-ancestor 9a1d3e4969 HEAD`. If it fails, tell the user that the branch needs master merged. On such a branch, a pushed commit whose message contains `[app-ci]`, `[claude-test]`, `[ci-backend]` or `[ci-release]` (or a form like `[app-ci:ios]`) starts a run: never write these.
2. The run tests the branch as it is in the fork: `git push -u <remote> HEAD`, then `git fetch <remote> <branch>`; `FETCH_HEAD` must equal `HEAD`.
3. `gh workflow run <workflow>.yml --repo <owner>/simplex-chat --ref <branch>` with `ios-sim`, `desktop-linux` or `android-emu`, plus `-f claude=true`, `-f backend=true` or `-f release=true` as needed. If `gh` can't find or start the workflow, the fork isn't set up for it: tell the user.
4. A few seconds later, `gh run list --repo <owner>/simplex-chat --workflow <workflow>.yml --branch <branch> --event workflow_dispatch --limit 1 --json databaseId,createdAt,headSha,url` gives its id: check that it was created after you started it and that `headSha` is your HEAD. Wait in the background with `gh run watch <id> --repo <owner>/simplex-chat --exit-status`.

Start only what the check needs: the platforms the change touches; `claude` for what the scripted test can't check; `backend` when the test needs servers, the directory, names or peers, and whenever `claude-scope` says `backend: yes` (a Claude run fails without it); `release` (desktop, Android) to tell the branch's failure from the runner's. While a branch's Claude plan is still master's smoke plan, Claude checks only onboarding and the chat list, nothing of the feature. A newer run of a workflow on a branch cancels the older one. Before pushing a core change, run the core tests for it locally (README, "core"): a runner is a much slower loop.

## Reading results

`gh run view <id> --repo <owner>/simplex-chat` lists the jobs (`--log-failed` adds the failed steps' logs); `gh run download <id> --repo <owner>/simplex-chat -n <artifact> -D <dir>` fetches an artifact (each platform's README names them; scripted results are kept 3 days, Claude's 7, on iOS 14). A Claude session passes when its final message ends with `CLAUDE-TEST: PASS`; its report, transcript, guard log and usage (`claude-usage.json`, on iOS each session's `usage.json`) are in the `*-claude-test` artifact. Tell the user each Claude run's turns and cost estimated at API prices.

## When a run fails

- An iOS job cancelled after about 15 minutes with "The job was not acquired by Runner of type hosted even after multiple attempts" got no macOS runner: start a new run with the same inputs, changing nothing.
- A branch's first run, and the first after its Haskell sources change (merging master too), builds the core: on a fork that took about 26 minutes in all on desktop, 45 on Android, 57 on iOS. Later runs reuse it for 30 days.
- The Android app runs under ARM translation, which can crash it at start or drop typed text: compare with a `release=true` run before calling a failure an app bug.
- A Claude job failing at the environment check means the fork's `app-sim-claude` environment doesn't allow the branch. If the Claude step can't authenticate or hits a usage limit, stop and tell the user, who checks the environment's branch rule and secret: never retry in a loop or look for another credential.

## Changing the tests

- Check both chat list layouts (toolbar at the bottom, the default, called `reachable` in `claude-scope`, and at the top) only when the change can affect layout (list structure or order, headers, toolbars) or a run found a layout problem there; texts, alerts, fonts and data need the default one. A Claude plan runs in one layout, and in the other only the cases the layouts render differently (iOS: `layouts` and `cases-<layout>` in `claude-scope`).
- A feature's Claude plan replaces `device-test-plan.md` (iOS) or `claude-test.md` (desktop, Android), and its `claude-scope`, on the feature branch. On desktop and Android, a prompt that uses the test backend also needs `claude-test.sh` to give the session the backend's addresses, as iOS's does; master's doesn't yet.
- One kind per commit: CI changes every branch would want, CI changes only this feature needs (its plans, which don't go to master), fixes to the feature, fixes to bugs master has too. Report each kind's commits and whether a runner verified them.
- A workflow you add or change triggers only on `workflow_dispatch`, which can start it once the file is on the fork's default branch. No `pull_request_target`, no write permissions for `GITHUB_TOKEN`, no secret but `CLAUDE_CODE_OAUTH_TOKEN`, and that only in the step that runs `claude`. Don't test branch names in workflow code: the environment's branch rule decides which branches get the token.
- The runners build simplexmq from simplex-chat/simplexmq at the commit `cabal.project` pins (the backend's build clones that repository): push a simplexmq change there first, and update `scripts/nix/sha256map.nix` with the new pin for Android's Nix build.

## Never

- Print, echo, encode, save or upload the Claude token or the Claude step's environment: the fork's logs and artifacts are public, and GitHub masks only the exact value.
- Push anything but the branch you are on, push `master`, `stable` or tags, or force-push.
