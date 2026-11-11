# WebView for WebRTC calls in SimpleX Chat

```
npm i
npm run build
```

## Tests

End-to-end tests run two headless Chromium peers with `call.js` and simulate network outages by relaying media via UDP sockets controlled by the test:

```
npx playwright-core install chromium
npm test
```

Set `CHROME` to use another Chromium executable, and `VERBOSE=1` to log signaling and connection states.

Full-stack tests in `test/fullstack.test.js` also pass call signaling via two chat cores and a local SMP server. They run when `SIMPLEX_CHAT` and `SMP_SERVER` are set to `simplex-chat` and `smp-server` executables:

```
SIMPLEX_CHAT=path/to/simplex-chat SMP_SERVER=path/to/smp-server npm test
```
