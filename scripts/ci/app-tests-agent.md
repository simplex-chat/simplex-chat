# App tests from a fork: instructions for agents

For an agent that tests the iOS, desktop or Android app with the app test workflows ([scripts/ci/README.md](README.md#starting-a-run), "Starting a run") in its user's fork of simplex-chat. The user tells you the fork (`<owner>/simplex-chat`), its git remote (`<remote>`), and whether you may start Claude runs, which spend their Claude subscription's usage limits. Your `gh` login needs write access to the fork. Pass `--repo <owner>/simplex-chat` to every `gh` command: with simplex-chat as another remote, `gh` can pick it.

## Starting a run

1. The branch needs master's app test workflows from 9a1d3e4969 (2026-10-20) or later: fetch master from the remote for simplex-chat, then `git merge-base --is-ancestor 9a1d3e4969 HEAD`. If it fails, tell the user the branch needs master merged.
2. The run tests the branch as it is in the fork: commit the work to test by explicit paths (never a `cabal.project` that points at a local simplexmq), `git push -u <remote> HEAD`, then `git fetch <remote> <branch>`; `FETCH_HEAD` must equal `HEAD`. These steps test the branch you are on; to test another, ask the user first.
3. `gh workflow run <workflow>.yml --repo <owner>/simplex-chat --ref <branch>` with `ios-sim`, `desktop-linux` or `android-emu`, plus `-f claude=true`, `-f backend=true` or `-f release=true` (`ios-sim` has no `release`). If `gh` can't find or start the workflow, ask the user to check the fork's setup: Actions on, the three workflows enabled and on its default branch, master.
4. A few seconds later, `gh run list --repo <owner>/simplex-chat --workflow <workflow>.yml --branch <branch> --event workflow_dispatch --limit 1 --json databaseId,createdAt,headSha,url` gives its id: check that it was created after you started it and that `headSha` is your HEAD. Wait in the background with `gh run watch <id> --repo <owner>/simplex-chat --exit-status --interval 60`.

Start only what the check needs:

- the platforms the change touches;
- `claude` for what the scripted test can't check. That test covers onboarding and the chat list in both layouts, and master's Claude plans are short smoke plans that add little until the branch has its own;
- `backend` when the test uses the test backend's servers, directory or names (each platform's README says what it seeds). On iOS only Claude uses it, so start it there only with `claude`. Whenever the platform's `claude-scope` says `backend: yes`, a Claude run needs it or fails;
- `release` (desktop, Android) to tell the branch's failure from the runner's.

A newer run of a workflow on a branch cancels the older one, whose watch then exits non-zero without a failure. To test new commits, start a new run: `gh run rerun` tests the old commit. Run the core tests for a core change locally before pushing it (README, "core"): a runner is a much slower loop.

## Reading results

`gh run view <id> --repo <owner>/simplex-chat` lists the jobs, and with `--log-failed` prints the failed steps' logs. `gh run download <id> --repo <owner>/simplex-chat -n <artifact> -D <dir>` fetches an artifact; each platform's README names them. Scripted results are kept 3 days, Claude's 7 (on iOS 14). A Claude session passes when its final message ends with `CLAUDE-TEST: PASS`; its report, transcript, guard log and usage (`claude-usage.json`) are in the `*-claude-test` artifact. Tell the user each Claude run's turns and cost estimated at API prices; a cancelled desktop or Android Claude job leaves no usage, so its cost is unknown.

## When a run fails

- An iOS job cancelled after about 15 minutes with "The job was not acquired by Runner of type hosted even after multiple attempts" got no macOS runner: start a new run with the same inputs (the plan job's log prints them), changing nothing. If it happens again, tell the user.
- A branch's first run, and the first after the core's sources change (a master merge too), builds the core, which takes longest on iOS (README, "How long a run takes"). Later runs of the branch reuse it for 30 days.
- The Android app runs under ARM translation, which can crash it at start or drop typed text: compare with a `release=true` run before calling a failure an app bug.
- Claude failures are the user's to fix, in the fork's `app-sim-claude` environment: failing at the environment check means its branch rule doesn't allow the branch; "CLAUDE_CODE_OAUTH_TOKEN must be set" means it has no token secret; an authentication error means the token expired or is wrong; a usage limit means waiting. Tell the user; never retry in a loop or look for another credential.

## Changing the tests

- Check both chat list layouts (toolbar at the bottom, the default, called `reachable` in `claude-scope`, and at the top) only when the change can affect layout (list structure or order, headers, toolbars) or a run found a layout problem there; texts, alerts, fonts and data need the default one. A Claude plan runs in one layout, and in the other only the cases the layouts render differently (iOS: `layouts` and `cases-<layout>` in `claude-scope`).
- A feature's Claude plan replaces `device-test-plan.md` (iOS) or `claude-test.md` (desktop, Android), with its `claude-scope`, on the feature branch. iOS sets each session's turn and budget limits in `claude-scope`. Desktop and Android set them (60 turns, $3) in desktop's `claude-test.sh`, which doesn't yet give the session the test backend's addresses, as iOS's does.
- One kind per commit: CI changes every branch would want, CI changes only this feature needs (its plans, which don't go to master), fixes to the feature, fixes to bugs master has too. Report each kind's commits and whether a runner verified them.
- A workflow you add or change triggers only on `workflow_dispatch`, which can start it once the file is on the fork's default branch. No `pull_request_target`, no write permissions for `GITHUB_TOKEN`, no secret but `CLAUDE_CODE_OAUTH_TOKEN`, and that only in the step that runs `claude`. Don't test branch names in workflow code: the environment's branch rule decides which branches get the token.
- The runners build simplexmq at the commit `cabal.project` pins, from simplex-chat/simplexmq (the backend's build fetches it from there). A branch that needs a simplexmq change runs only once that commit is there, the pin points at it and `scripts/nix/sha256map.nix` has its hash (Android's Nix build). Getting the commit into simplex-chat/simplexmq is the user's step: ask.

## Never

- Print, echo, encode, save or upload the Claude token or the Claude step's environment (no `set -x` in its scripts): the fork's logs and artifacts are public, and GitHub masks only the exact value.
- Write run tags in a commit message: `[app-ci]`, `[claude-test]`, their `:ios` and `:desktop` forms, `[ci-backend]`, `[ci-release]`. On a branch whose workflows predate 5834179be6 (#7777), a push starts their runs, whose jobs run when the pushed commit's message has one of the first ones; the last two add jobs.
- Push to or start runs in simplex-chat/simplex-chat itself: only in the fork.
- Push anything but the branch you are on, push `master`, `stable` or tags, or force-push.
