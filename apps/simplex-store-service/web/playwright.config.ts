import { defineConfig, devices } from "@playwright/test";

const BASE_URL = process.env.BASE_URL;

export default defineConfig({
  testDir: "e2e",
  workers: 1,
  timeout: 180_000,
  expect: { timeout: 15_000 },
  reporter: [["list"]],
  use: { ...(BASE_URL === undefined ? {} : { baseURL: BASE_URL }), reducedMotion: "reduce" },
  projects: [
    { name: "chromium", use: devices["Desktop Chrome"] },
    { name: "firefox", use: devices["Desktop Firefox"] },
    { name: "webkit", use: devices["Desktop Safari"] },
    { name: "android", use: devices["Pixel 7"] },
    { name: "iphone", use: devices["iPhone 14"] },
  ],
});
