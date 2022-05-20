const Ws = require("isomorphic-ws") as typeof WebSocket

Object.defineProperty(global, "WebSocket", {value: Ws})
