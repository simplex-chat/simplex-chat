import { defineChannelPluginEntry } from "openclaw/plugin-sdk/channel-core";
import { simplexPlugin } from "./src/channel.js";
import { setSimplexRuntime } from "./src/runtime.js";

export default defineChannelPluginEntry({
  id: "simplex",
  name: "SimpleX",
  description: "SimpleX Chat channel: end-to-end encrypted direct messages",
  plugin: simplexPlugin,
  setRuntime: setSimplexRuntime,
});
