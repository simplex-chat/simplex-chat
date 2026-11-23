#!/usr/bin/env python3

# Starts the desktop app on macOS or Windows, completes onboarding, creates a group and opens each screen reachable from
# the chat list, saving each as <out dir>/NN-tour-<name>.png; the steps are the "tour" case of desktop-linux/smoke-test.sh.
# Usage: scripts/ci/desktop-tour.py <app executable> <out dir>

import ctypes
import re
import subprocess
import sys
import time
from pathlib import Path

import pyautogui
import pytesseract
from PIL import ImageChops, ImageFilter, ImageOps

OCR_SCALE = 2
STROKE_FILTER = 9
OCR_CONFIG = "--psm 11"
FIRST_SCREEN_WAIT = 180
STEP_WAIT = 10
NETWORK_WAIT = 60
SETTLE = 1
SCROLL_CLICKS = -10
LABEL_SCROLLS = 4
LABEL_WAIT = 5
LABEL_SCROLL_CLICKS = -3
TYPE_ATTEMPTS = 3
TITLE_LENGTH = 256
SW_MAXIMIZE = 3
LAYOUT_SETTLE = 3
BACK_PRESSES = 6
AVATAR_OFFSET = 27
BOTTOM_BAR_OFFSET = 30
PENCIL_OFFSET = 363
PANE_CENTRE_OFFSET = 195
PANE_RIGHT_OFFSET = 390
PAPERCLIP_OFFSET = 415
FIELD_BELOW_LABEL = 40
LOCAL_CHAT = "seeded notes"
PROFILE_NAME = "citester"
SEARCH_FIELD = "Search or paste SimpleX link"
PASTE_ITEM = "Paste"
CLIPBOARD_COMMAND = ["clip"] if sys.platform == "win32" else ["pbcopy"]
WHEEL_NOTCH = 120 if sys.platform == "win32" else 1
LEFT_PANE_SCREENS = ("Settings", "Chat preferences", "Network", "New chat")
GRADLE_PROPERTIES = Path(__file__).resolve().parents[2] / "apps" / "multiplatform" / "gradle.properties"
VERSION_ROW = "(%s)" % re.search(r"^desktop\.version_code=(.+)$", GRADLE_PROPERTIES.read_text(), re.MULTILINE).group(1).strip()

pyautogui.PAUSE = 0.2


def key(text):
    return re.sub(r"[^0-9a-z]", "", text.lower()).replace("l", "i")


class App:
    def __init__(self, executable, out):
        self.out = Path(out)
        self.out.mkdir(parents=True, exist_ok=True)
        self.shots = 0
        self.checks = self.out / "checks.txt"
        self.checks.write_text("")
        log = open(self.out / "app.log", "w")
        self.process = subprocess.Popen([executable], stdout=log, stderr=subprocess.STDOUT)
        self.x = self.bottom = 0

    def log(self, message):
        line = f"{time.strftime('%H:%M:%S')} {message}"
        print(line, flush=True)
        with open(self.out / "steps.log", "a") as f:
            f.write(line + "\n")

    def check(self, what, passed):
        line = f"{'PASS' if passed else 'FAIL'} {what}"
        print(line, flush=True)
        with open(self.checks, "a") as f:
            f.write(line + "\n")

    def phrases(self):
        shot = pyautogui.screenshot()
        gray = ImageOps.grayscale(shot).resize((shot.width * OCR_SCALE, shot.height * OCR_SCALE))
        opened = gray.filter(ImageFilter.MinFilter(STROKE_FILTER)).filter(ImageFilter.MaxFilter(STROKE_FILTER))
        light_text = ImageOps.invert(ImageOps.autocontrast(ImageChops.subtract(gray, opened)))
        found = {}
        for image in (ImageOps.autocontrast(gray), light_text):
            data = pytesseract.image_to_data(image, config=OCR_CONFIG, output_type=pytesseract.Output.DICT)
            lines = {}
            for i, word in enumerate(data["text"]):
                if not key(word or ""):
                    continue
                line = (data["block_num"][i], data["par_num"][i], data["line_num"][i])
                box = [data[k][i] / OCR_SCALE for k in ("left", "top", "width", "height")]
                lines.setdefault(line, []).append((word, box))
            for words in lines.values():
                text = " ".join(w for w, _ in words)
                left = min(b[0] for _, b in words)
                top = min(b[1] for _, b in words)
                right = max(b[0] + b[2] for _, b in words)
                bottom = max(b[1] + b[3] for _, b in words)
                centre = (int((left + right) / 2), int((top + bottom) / 2))
                found.setdefault((key(text), centre), (text, centre))
        return list(found.values())

    def find(self, text, max_x=None):
        target = key(text)
        candidates = [p for p in self.phrases() if max_x is None or p[1][0] < max_x]
        for phrase, centre in candidates:
            if key(phrase) == target:
                return centre
        for phrase, centre in candidates:
            if target in key(phrase):
                return centre
        return None

    def wait(self, text, secs=STEP_WAIT):
        deadline = time.time() + secs
        while True:
            centre = self.find(text)
            if centre or time.time() > deadline:
                self.log(f"wait \"{text}\": {centre or 'not on screen'}")
                return centre
            time.sleep(SETTLE)

    def click(self, x, y):
        self.log(f"click {x} {y}")
        pyautogui.moveTo(x, y)
        pyautogui.click(x, y)

    def click_text(self, text, secs=STEP_WAIT):
        centre = self.wait(text, secs)
        if centre:
            self.click(*centre)
        return centre is not None

    def type(self, text):
        self.log(f"type {text}")
        pyautogui.write(text, interval=0.05)

    def press(self, name):
        self.log(f"key {name}")
        pyautogui.press(name)

    def scroll(self, x, y, clicks):
        pyautogui.moveTo(x, y)
        pyautogui.scroll(clicks * WHEEL_NOTCH)

    def shot(self, name):
        self.shots += 1
        path = self.out / f"{self.shots:02d}-{name}.png"
        pyautogui.screenshot().save(path)
        self.log(f"shot {path.name}")

    def window_rect(self, maximize=False):
        if sys.platform == "win32":
            import ctypes.wintypes
            user32 = ctypes.windll.user32
            hwnd = user32.FindWindowW(None, "SimpleX")
            if not hwnd:
                return None
            if maximize:
                user32.ShowWindow(hwnd, SW_MAXIMIZE)
            user32.SetForegroundWindow(hwnd)
            time.sleep(SETTLE)
            rect = ctypes.wintypes.RECT()
            user32.GetClientRect(hwnd, ctypes.byref(rect))
            origin = ctypes.wintypes.POINT(0, 0)
            user32.ClientToScreen(hwnd, ctypes.byref(origin))
            return origin.x, origin.y, rect.right, rect.bottom
        script = (
            'tell application "System Events" to tell (first process whose unix id is %d)\n'
            "set frontmost to true\n"
            "set {x, y} to position of window 1\n"
            "set {w, h} to size of window 1\n"
            'return (x as text) & " " & (y as text) & " " & (w as text) & " " & (h as text)\n'
            "end tell" % self.process.pid
        )
        result = subprocess.run(["osascript", "-e", script], capture_output=True, text=True)
        values = result.stdout.split()
        return tuple(int(v) for v in values) if len(values) == 4 else None

    def use_rect(self, rect):
        self.x, top, width, height = rect
        self.bottom = top + height - BOTTOM_BAR_OFFSET
        self.log(f"window {rect}, maximized: {self.maximized()}")

    def maximized(self):
        if sys.platform != "win32":
            return None
        user32 = ctypes.windll.user32
        return bool(user32.IsZoomed(user32.FindWindowW(None, "SimpleX")))

    def await_window(self, secs=FIRST_SCREEN_WAIT):
        deadline = time.time() + secs
        while time.time() < deadline:
            rect = self.window_rect()
            if rect and rect[2] > 0:
                self.use_rect(rect)
                return True
            time.sleep(SETTLE)
        return False

    def maximize(self):
        self.window_rect(maximize=True)
        time.sleep(LAYOUT_SETTLE)
        rect = self.window_rect()
        if rect:
            self.use_rect(rect)

    def on_chat_list(self):
        phrases = [key(p) for p, c in self.phrases() if c[0] < self.x + PANE_RIGHT_OFFSET]
        if any(key(s) in phrases for s in LEFT_PANE_SCREENS):
            return False
        return any(key(SEARCH_FIELD) in p for p in phrases)

    def back_to_chat_list(self):
        for attempt in range(BACK_PRESSES):
            if self.on_chat_list():
                return True
            if attempt % 2:
                self.click(self.x + AVATAR_OFFSET, self.bottom)
            else:
                self.press("escape")
            time.sleep(SETTLE)
        return self.on_chat_list()

    def foreground_title(self):
        if sys.platform != "win32":
            return ""
        user32 = ctypes.windll.user32
        title = ctypes.create_unicode_buffer(TITLE_LENGTH)
        user32.GetWindowTextW(user32.GetForegroundWindow(), title, TITLE_LENGTH)
        return title.value

    def type_into(self, x, y, text):
        for attempt in range(TYPE_ATTEMPTS):
            self.window_rect()
            self.click(x, y)
            time.sleep(SETTLE)
            self.click(x, y)
            time.sleep(SETTLE)
            if attempt > 0:
                self.press("tab")
            self.log(f"foreground window: {self.foreground_title()}")
            self.type(text)
            if self.wait(text, LABEL_WAIT):
                return True
            self.shot(f"typing-{attempt + 1}")
        return self.paste_into(x, y, text)

    def paste_into(self, x, y, text):
        subprocess.run(CLIPBOARD_COMMAND, input=text, text=True, check=True)
        self.log(f"paste {text}")
        pyautogui.rightClick(x, y)
        if not self.click_text(PASTE_ITEM, LABEL_WAIT):
            self.shot("paste-menu")
            return False
        return self.wait(text, LABEL_WAIT) is not None

    def open_user_menu(self):
        self.click(self.x + AVATAR_OFFSET, self.bottom)
        return self.wait("Settings") is not None

    def open_settings(self):
        return self.open_user_menu() and self.click_text("Settings")

    def open_new_chat(self):
        self.click(self.x + PENCIL_OFFSET, self.bottom)
        return self.wait("New chat") is not None

    def open_local_chat(self):
        centre = self.find(LOCAL_CHAT, self.x + PANE_RIGHT_OFFSET)
        if centre:
            self.click(*centre)
        return centre is not None

    def open_group_info(self):
        if not self.open_local_chat():
            return False
        time.sleep(SETTLE)
        for phrase, centre in self.phrases():
            if centre[0] >= self.x + PANE_RIGHT_OFFSET and key(phrase) == key(LOCAL_CHAT):
                self.click(*centre)
                return True
        return False

    def click_scrolled(self, text):
        for _ in range(LABEL_SCROLLS):
            if self.click_text(text, LABEL_WAIT):
                return True
            self.scroll(self.x + PANE_CENTRE_OFFSET, self.bottom - PANE_RIGHT_OFFSET, LABEL_SCROLL_CLICKS)
        return False

    def visit(self, name, opener, *labels):
        self.back_to_chat_list()
        opened = opener() and all(self.click_scrolled(label) for label in labels)
        if opened:
            time.sleep(SETTLE)
            self.shot(f"tour-{name}")
            self.scroll(self.x + PANE_CENTRE_OFFSET, self.bottom - PANE_RIGHT_OFFSET, SCROLL_CLICKS)
            time.sleep(SETTLE)
            self.shot(f"tour-{name}-end")
        self.check(f"tour: {name} opens", opened)

    def onboarding(self):
        if not self.wait("Create your profile", FIRST_SCREEN_WAIT):
            return False
        self.maximize()
        if not self.click_text("Create your profile"):
            return False
        field = self.wait("Enter profile name", NETWORK_WAIT)
        if not field or not self.type_into(*field, PROFILE_NAME):
            return False
        self.shot("profile-name")
        for text in ("Create profile", "Use random passphrase", "OK", "Continue", "Accept"):
            if not self.click_text(text, NETWORK_WAIT):
                return False
        return self.wait("Talk to someone", NETWORK_WAIT) is not None

    def create_local_chat(self):
        if not self.open_new_chat() or not self.click_text("Create group"):
            return False
        label = self.wait("Enter group name")
        if not label:
            return False
        if not self.type_into(self.x + PANE_CENTRE_OFFSET, label[1] + FIELD_BELOW_LABEL, LOCAL_CHAT):
            return False
        if not self.click_text("Create group"):
            return False
        self.click_text("Skip inviting members")
        self.press("escape")
        return self.back_to_chat_list()

    def tour(self):
        self.visit("user-menu", self.open_user_menu)
        self.visit("settings", self.open_settings)
        self.visit("appearance", self.open_settings, "Appearance")
        self.visit("privacy", self.open_settings, "Your privacy")
        self.visit("help", self.open_settings, "Help & support")
        self.visit("how-to-use", self.open_settings, "Help & support", "How to use it")
        self.visit("whats-new", self.open_settings, "Help & support", "What's new")
        self.visit("about", self.open_settings, "Help & support", "About SimpleX Chat")
        self.visit("chat-data", self.open_settings, "Chat data")
        self.visit("support", self.open_settings, "Support SimpleX")
        self.visit("network", self.open_settings, "Network")
        self.visit("your-servers", self.open_settings, "Network", "Your servers")
        self.visit("advanced-network", self.open_settings, "Network", "Advanced network settings")
        self.visit("calls", self.open_settings, "Audio")
        self.visit("address", self.open_user_menu, "SimpleX address")
        self.visit("chat-preferences", self.open_user_menu, "Chat preferences")
        self.visit("chat-profiles", self.open_user_menu, "Your chat profiles")
        self.visit("link-mobile", self.open_user_menu, "Link a mobile")
        self.visit("new-chat", self.open_new_chat)
        self.visit("one-time-link", self.open_new_chat, "Create 1-time link")
        self.visit("local-chat", self.open_local_chat)
        self.visit("group-info", self.open_group_info)
        self.back_to_chat_list()
        if self.open_local_chat():
            self.click(self.x + PAPERCLIP_OFFSET, self.bottom)
            time.sleep(SETTLE)
            self.shot("tour-attachment-menu")
            self.press("escape")
        self.visit("version", self.open_settings, VERSION_ROW)
        self.visit("developer-tools", self.open_settings, VERSION_ROW, "Developer tools")
        self.visit("migrate", self.open_settings, "Migrate to another device")

    def stop(self):
        self.process.terminate()
        try:
            self.process.wait(STEP_WAIT)
        except subprocess.TimeoutExpired:
            self.process.kill()


def main(executable, out):
    app = App(executable, out)
    try:
        started = app.await_window()
        app.check("the app window shows", started)
        app.shot("first-screen")
        onboarded = started and app.onboarding()
        app.check("onboarding reaches the chat list", onboarded)
        app.shot("chat-list")
        created = onboarded and app.create_local_chat()
        app.check(f"the group \"{LOCAL_CHAT}\" is created", created)
        if created:
            app.tour()
    finally:
        app.shot("final")
        app.stop()
    failed = sum(1 for line in app.checks.read_text().splitlines() if line.startswith("FAIL"))
    print(f"tour done, {failed} checks failed")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(*sys.argv[1:3]))
