import type { PluginRuntime } from "openclaw/plugin-sdk/core";
import { createPluginRuntimeStore } from "openclaw/plugin-sdk/runtime-store";

const { setRuntime: setSimplexRuntime, getRuntime: getSimplexRuntime } =
  createPluginRuntimeStore<PluginRuntime>({
    pluginId: "simplex",
    errorMessage: "SimpleX runtime not initialized",
  });

export { getSimplexRuntime, setSimplexRuntime };
