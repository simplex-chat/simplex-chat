const CHROMIUM_OR_ANDROID = /Chrome\/|Chromium\/|Android/;

export function browserDeletesIdle(userAgent: string): boolean {
  return userAgent.includes("AppleWebKit/") && !CHROMIUM_OR_ANDROID.test(userAgent);
}
