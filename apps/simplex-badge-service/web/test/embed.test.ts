import assert from "node:assert/strict";
import { test } from "node:test";
import { THEME_MESSAGE, themeFromMessage, trustedHost } from "../src/embed.js";

test("embed: only https simplex.chat and its subdomains may drive the theme", () => {
  for (const origin of ["https://simplex.chat", "https://www.simplex.chat", "https://badges.simplex.chat"]) {
    assert.equal(trustedHost(origin), true, `${origin} is the site`);
  }
  for (const origin of [
    "http://simplex.chat",                 // not https
    "https://simplex.chat.attacker.com",   // suffix, not the site
    "https://notsimplex.chat",             // no dot before the suffix
    "https://evil.com",
    "null",                                // a sandboxed frame's opaque origin
    "",
  ]) {
    assert.equal(trustedHost(origin), false, `${origin} is not the site`);
  }
});

test("embed: a theme message yields its theme, and anything else yields undefined", () => {
  for (const theme of ["light", "dark", "system"]) {
    assert.equal(themeFromMessage({ type: THEME_MESSAGE, theme }), theme, `${theme} is a theme`);
  }
  for (const data of [
    { type: "other", theme: "dark" },      // not our message
    { type: THEME_MESSAGE, theme: "neon" },// not a theme this build has
    { type: THEME_MESSAGE },               // no theme
    { theme: "dark" },                     // no type
    "dark", 42, null, undefined, [THEME_MESSAGE],
  ]) {
    assert.equal(themeFromMessage(data), undefined, `${JSON.stringify(data)} carries no theme`);
  }
});
