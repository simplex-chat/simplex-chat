import { timedTest } from "./boot.js";
import assert from "node:assert/strict";

const smokeTest = timedTest(2000);


smokeTest("the toolchain runs ES modules under node:test", () => {
  assert.equal(typeof crypto.subtle.digest, "function");
});
