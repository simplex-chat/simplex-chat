#!/usr/bin/env python3

import argparse
import ctypes
import os
import re
import subprocess
import sys
import tempfile
import time
from ctypes import CFUNCTYPE, POINTER, Structure, byref, c_char, c_char_p, c_float, c_int, c_int64, c_size_t, c_uint, c_uint32, c_void_p
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
ALLOWLIST = ROOT / "scripts/desktop/vlc-plugins.txt"
APP_SOUNDS = ROOT / "apps/multiplatform/common/src/desktopMain/resources/media"
APP_PLAYER = ROOT / "apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/RecAndPlay.desktop.kt"
APP_DEMUX = re.search(r'const val VLC_DEMUX = "(--demux=([^"]+))"', APP_PLAYER.read_text())
PREVIEW_ARGS = ("--avcodec-hw=none", APP_DEMUX[1])
PLAYER_ARGS = (APP_DEMUX[1],)
DEMUX_MODULES = {"main", *APP_DEMUX[2].split(",")}
ALLOWED_MODULES = {"demux": DEMUX_MODULES, "stream": {*DEMUX_MODULES, "filesystem", "cache_read", "skiptags"}}
LOG_WARNING = 3
VOICE_SOUT = "#transcode{vcodec=none,acodec=mp4a,ab=32,channels=1,samplerate=16000}:std{access=file,mux=mp4,dst=%s}"
VOICE_CHANNELS = 1
VOICE_RATE = 16000
SECONDS = 2
FRAMES = 3
PLAY_TIMEOUT = 30
PARSE_TIMEOUT_MS = 10000
POLL_SECONDS = 0.05
PARSE_LOCAL = 0
PARSED_DONE = 4
STATE_ENDED = 6
STATE_ERROR = 7
TRACK_AUDIO = 0
TRACK_VIDEO = 1
ORIENT_SWAP = 4
VIDEO_SOURCE = f"testsrc2=size=320x240:rate=25:duration={SECONDS}"
AUDIO_SOURCE = f"sine=frequency=440:sample_rate=48000:duration={SECONDS}"
STEREO = ("-ac", "2")
H264 = ("-c:v", "libx264", "-pix_fmt", "yuv420p")


@dataclass(frozen=True)
class Media:
    name: str
    video: bool
    portrait: bool
    ffmpeg: tuple[str, ...]
    copy_of: str | None = None
    played: bool = True


GENERATED = (
    Media("voice.m4a", False, False, ("-c:a", "aac", "-ac", "1", "-ar", "16000", "-b:a", "32k")),
    Media("h264.mp4", True, False, (*H264, "-c:a", "aac", *STEREO)),
    Media("rotated.mp4", True, True, ("-c", "copy", "-metadata:s:v:0", "rotate=90"), copy_of="h264.mp4"),
    Media("hevc.mp4", True, False, ("-c:v", "libx265", "-tag:v", "hvc1", "-pix_fmt", "yuv420p", "-c:a", "aac", *STEREO)),
    Media("av1.mp4", True, False, ("-c:v", "libaom-av1", "-cpu-used", "8", "-crf", "50", "-b:v", "0", "-pix_fmt", "yuv420p", "-c:a", "aac", *STEREO)),
    Media("h264.mov", True, False, (*H264, "-c:a", "aac", *STEREO)),
    Media("h263.3gp", True, False, ("-s", "176x144", "-c:v", "h263", "-c:a", "aac", "-ar", "8000", "-ac", "1")),
    Media("vp9.webm", True, False, ("-c:v", "libvpx-vp9", "-pix_fmt", "yuv420p", "-c:a", "libopus", *STEREO)),
    Media("vp8.webm", True, False, ("-c:v", "libvpx", "-pix_fmt", "yuv420p", "-c:a", "libvorbis", *STEREO)),
    Media("h264.mkv", True, False, (*H264, "-c:a", "ac3", *STEREO)),
    Media("mpeg4.avi", True, False, ("-c:v", "mpeg4", "-pix_fmt", "yuv420p", "-c:a", "libmp3lame", *STEREO)),
    Media("mpeg1.mpg", True, False, ("-c:v", "mpeg1video", "-pix_fmt", "yuv420p", "-c:a", "mp2", *STEREO)),
    Media("mpeg2.mpeg", True, False, ("-f", "vob", "-c:v", "mpeg2video", "-pix_fmt", "yuv420p", "-c:a", "mp2", *STEREO)),
    Media("other.flv", True, False, (*H264, "-c:a", "aac", *STEREO), played=False),
)
SOUNDS = tuple(Media(sound.name, False, False, ()) for sound in sorted(APP_SOUNDS.glob("*.mp3")))


class Track(Structure):
    _fields_ = [
        ("codec", c_uint32),
        ("original_fourcc", c_uint32),
        ("id", c_int),
        ("type", c_int),
        ("profile", c_int),
        ("level", c_int),
        ("details", c_void_p),
    ]


class AudioTrack(Structure):
    _fields_ = [("channels", c_uint), ("rate", c_uint)]


class VideoTrack(Structure):
    _fields_ = [
        ("height", c_uint),
        ("width", c_uint),
        ("sar_num", c_uint),
        ("sar_den", c_uint),
        ("frame_rate_num", c_uint),
        ("frame_rate_den", c_uint),
        ("orientation", c_int),
    ]


@dataclass(frozen=True)
class TrackInfo:
    type: int
    codec: str
    channels: int = 0
    rate: int = 0
    orientation: int = 0


@dataclass(frozen=True)
class Parsed:
    status: int
    duration: int
    tracks: list[TrackInfo]


@dataclass(frozen=True)
class Playback:
    state: int
    tracks: list[TrackInfo]
    decoded_audio: int
    played_audio: int


class Stats(Structure):
    _fields_ = [
        ("read_bytes", c_int),
        ("input_bitrate", c_float),
        ("demux_read_bytes", c_int),
        ("demux_bitrate", c_float),
        ("demux_corrupted", c_int),
        ("demux_discontinuity", c_int),
        ("decoded_video", c_int),
        ("decoded_audio", c_int),
        ("displayed_pictures", c_int),
        ("lost_pictures", c_int),
        ("played_abuffers", c_int),
        ("lost_abuffers", c_int),
        ("sent_packets", c_int),
        ("sent_bytes", c_int),
        ("send_bitrate", c_float),
    ]


LOG = CFUNCTYPE(None, c_void_p, c_int, c_void_p, c_char_p, c_void_p)
LOCK = CFUNCTYPE(c_void_p, c_void_p, POINTER(c_void_p))
UNLOCK = CFUNCTYPE(None, c_void_p, c_void_p, POINTER(c_void_p))
DISPLAY = CFUNCTYPE(None, c_void_p, c_void_p)
SETUP = CFUNCTYPE(c_uint, POINTER(c_void_p), POINTER(c_char), POINTER(c_uint), POINTER(c_uint), POINTER(c_uint), POINTER(c_uint))
CLEANUP = CFUNCTYPE(None, c_void_p)

SIGNATURES = {
    "libvlc_get_version": (c_char_p, []),
    "libvlc_new": (c_void_p, [c_int, POINTER(c_char_p)]),
    "libvlc_release": (None, [c_void_p]),
    "libvlc_log_set": (None, [c_void_p, LOG, c_void_p]),
    "libvlc_log_unset": (None, [c_void_p]),
    "libvlc_log_get_context": (None, [c_void_p, POINTER(c_char_p), POINTER(c_char_p), POINTER(c_uint)]),
    "libvlc_log_get_object": (None, [c_void_p, POINTER(c_char_p), POINTER(c_char_p), POINTER(c_size_t)]),
    "libvlc_media_get_stats": (c_int, [c_void_p, POINTER(Stats)]),
    "libvlc_media_new_path": (c_void_p, [c_void_p, c_char_p]),
    "libvlc_media_add_option": (None, [c_void_p, c_char_p]),
    "libvlc_media_parse_with_options": (c_int, [c_void_p, c_int, c_int]),
    "libvlc_media_get_parsed_status": (c_int, [c_void_p]),
    "libvlc_media_get_duration": (c_int64, [c_void_p]),
    "libvlc_media_tracks_get": (c_uint, [c_void_p, POINTER(POINTER(POINTER(Track)))]),
    "libvlc_media_tracks_release": (None, [POINTER(POINTER(Track)), c_uint]),
    "libvlc_media_release": (None, [c_void_p]),
    "libvlc_media_player_new_from_media": (c_void_p, [c_void_p]),
    "libvlc_video_set_callbacks": (None, [c_void_p, LOCK, UNLOCK, DISPLAY, c_void_p]),
    "libvlc_video_set_format_callbacks": (None, [c_void_p, SETUP, CLEANUP]),
    "libvlc_media_player_play": (c_int, [c_void_p]),
    "libvlc_media_player_get_state": (c_int, [c_void_p]),
    "libvlc_media_player_stop": (None, [c_void_p]),
    "libvlc_media_player_release": (None, [c_void_p]),
}


def fourcc(code: int) -> str:
    return code.to_bytes(4, "little").decode("latin-1")


def plugin_name(file: Path) -> str:
    return file.name.removeprefix("lib").split("_plugin.")[0]


def is_plugin(file: Path) -> bool:
    return file.name.startswith("lib") and "_plugin." in file.name


def make_media(directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    for media in GENERATED:
        if media.copy_of:
            sources = ["-i", str(directory / media.copy_of)]
        elif media.video:
            sources = ["-f", "lavfi", "-i", VIDEO_SOURCE, "-f", "lavfi", "-i", AUDIO_SOURCE]
        else:
            sources = ["-f", "lavfi", "-i", AUDIO_SOURCE]
        command = ["ffmpeg", "-hide_banner", "-loglevel", "error", "-y", *sources, *media.ffmpeg, str(directory / media.name)]
        print(" ".join(command), flush=True)
        subprocess.run(command, check=True)


def check_bundle(vlc_dir: Path) -> list[str]:
    allowed = set(ALLOWLIST.read_text().split())
    plugins_dir = vlc_dir / "vlc" / "plugins"
    shipped = {plugin_name(file) for file in vlc_dir.rglob("*") if file.is_file() and is_plugin(file)}
    print(f"{len(shipped)} plugins in {vlc_dir}: {' '.join(sorted(shipped))}", flush=True)
    failures = [f"plugin outside the allowlist: {name}" for name in sorted(shipped - allowed)]
    failures += [
        f"file in the plugins directory that is not a plugin: {file.relative_to(vlc_dir)}"
        for file in sorted(plugins_dir.rglob("*"))
        if file.is_file() and not is_plugin(file) and file.name != "plugins.dat"
    ]
    failures += [f"Lua script: {file.relative_to(vlc_dir)}" for file in sorted(vlc_dir.rglob("*.lua*"))]
    if not shipped:
        failures.append(f"no plugins in {vlc_dir}")
    return failures


def load_libvlc(vlc_dir: Path) -> ctypes.CDLL:
    plugin_path = str(vlc_dir / "vlc" / "plugins")
    os.environ["VLC_PLUGIN_PATH"] = plugin_path
    if sys.platform == "win32":
        ctypes.cdll.msvcrt._putenv(f"VLC_PLUGIN_PATH={plugin_path}".encode())
        os.add_dll_directory(str(vlc_dir))
        ctypes.CDLL(str(vlc_dir / "libvlccore.dll"))
        libvlc = ctypes.CDLL(str(vlc_dir / "libvlc.dll"))
    elif sys.platform == "darwin":
        libvlc = ctypes.CDLL(str(vlc_dir / "libvlc.dylib"))
    else:
        libvlc = ctypes.CDLL(str(vlc_dir / "libvlc.so"))
    for name, (restype, argtypes) in SIGNATURES.items():
        function = getattr(libvlc, name)
        function.restype = restype
        function.argtypes = argtypes
    return libvlc


class Frames:
    def __init__(self) -> None:
        self.count = 0
        self.size = (0, 0)
        self.buffer = ctypes.create_string_buffer(0)
        self.setup = SETUP(self.on_setup)
        self.cleanup = CLEANUP(self.on_cleanup)
        self.lock = LOCK(self.on_lock)
        self.unlock = UNLOCK(self.on_unlock)
        self.display = DISPLAY(self.on_display)

    def on_setup(self, opaque, chroma, width, height, pitches, lines) -> int:
        ctypes.memmove(chroma, b"RV32", 4)
        self.size = (width[0], height[0])
        pitches[0] = width[0] * 4
        lines[0] = height[0]
        self.buffer = ctypes.create_string_buffer(pitches[0] * lines[0])
        return 1

    def on_cleanup(self, opaque) -> None:
        pass

    def on_lock(self, opaque, planes) -> None:
        planes[0] = ctypes.addressof(self.buffer)

    def on_unlock(self, opaque, picture, planes) -> None:
        pass

    def on_display(self, opaque, picture) -> None:
        self.count += 1


class Player:
    def __init__(self, libvlc: ctypes.CDLL, args: tuple[str, ...]) -> None:
        self.libvlc = libvlc
        self.modules: set[tuple[str, str]] = set()
        argv = (c_char_p * len(args))(*(arg.encode() for arg in args))
        self.instance = libvlc.libvlc_new(len(args), argv)
        if not self.instance:
            raise RuntimeError(f"libvlc_new failed with {' '.join(args)}")
        self.log = LOG(self.on_log)
        libvlc.libvlc_log_set(self.instance, self.log, None)

    def on_log(self, data, level, context, text, args) -> None:
        module, file, object_type = c_char_p(), c_char_p(), c_char_p()
        line = c_uint()
        self.libvlc.libvlc_log_get_context(context, byref(module), byref(file), byref(line))
        self.libvlc.libvlc_log_get_object(context, byref(object_type), None, None)
        names = ((object_type.value or b"").decode(), (module.value or b"").decode())
        self.modules.add(names)
        if level >= LOG_WARNING:
            print(f"[{names[1]} {names[0]}] {(text or b'').decode(errors='replace').strip()}", flush=True)

    def release(self) -> None:
        self.libvlc.libvlc_log_unset(self.instance)
        self.libvlc.libvlc_release(self.instance)

    def unexpected_modules(self) -> list[str]:
        return [
            f"{object_type} module {module} outside {sorted(ALLOWED_MODULES[object_type])}"
            for object_type, module in sorted(self.modules)
            if object_type in ALLOWED_MODULES and module not in ALLOWED_MODULES[object_type]
        ]

    def media(self, path: Path, *options: str) -> int:
        media = self.libvlc.libvlc_media_new_path(self.instance, str(path).encode())
        for option in options:
            self.libvlc.libvlc_media_add_option(media, option.encode())
        return media

    def tracks(self, media: int) -> list[TrackInfo]:
        tracks = POINTER(POINTER(Track))()
        count = self.libvlc.libvlc_media_tracks_get(media, byref(tracks))
        found = []
        for i in range(count):
            track = tracks[i].contents
            info = TrackInfo(track.type, fourcc(track.codec))
            if track.type == TRACK_AUDIO and track.details:
                audio = ctypes.cast(track.details, POINTER(AudioTrack)).contents
                info = TrackInfo(track.type, info.codec, channels=audio.channels, rate=audio.rate)
            elif track.type == TRACK_VIDEO and track.details:
                video = ctypes.cast(track.details, POINTER(VideoTrack)).contents
                info = TrackInfo(track.type, info.codec, orientation=video.orientation)
            found.append(info)
        if count:
            self.libvlc.libvlc_media_tracks_release(tracks, count)
        return found

    def parse(self, path: Path) -> Parsed:
        media = self.media(path)
        self.libvlc.libvlc_media_parse_with_options(media, PARSE_LOCAL, PARSE_TIMEOUT_MS)
        deadline = time.monotonic() + PARSE_TIMEOUT_MS / 1000
        while self.libvlc.libvlc_media_get_parsed_status(media) == 0 and time.monotonic() < deadline:
            time.sleep(POLL_SECONDS)
        parsed = Parsed(self.libvlc.libvlc_media_get_parsed_status(media), self.libvlc.libvlc_media_get_duration(media), self.tracks(media))
        self.libvlc.libvlc_media_release(media)
        return parsed

    def play(self, media: int, frames: Frames | None = None) -> Playback:
        player = self.libvlc.libvlc_media_player_new_from_media(media)
        if frames is not None:
            self.libvlc.libvlc_video_set_callbacks(player, frames.lock, frames.unlock, frames.display, None)
            self.libvlc.libvlc_video_set_format_callbacks(player, frames.setup, frames.cleanup)
        self.libvlc.libvlc_media_player_play(player)
        deadline = time.monotonic() + PLAY_TIMEOUT
        state = self.libvlc.libvlc_media_player_get_state(player)
        while state not in (STATE_ENDED, STATE_ERROR) and time.monotonic() < deadline and (frames is None or frames.count < FRAMES):
            time.sleep(POLL_SECONDS)
            state = self.libvlc.libvlc_media_player_get_state(player)
        stats = Stats()
        self.libvlc.libvlc_media_get_stats(media, byref(stats))
        playback = Playback(state, self.tracks(media), stats.decoded_audio, stats.played_abuffers)
        self.libvlc.libvlc_media_player_stop(player)
        self.libvlc.libvlc_media_player_release(player)
        self.libvlc.libvlc_media_release(media)
        return playback

    def show_frames(self, path: Path) -> tuple[Playback, Frames]:
        frames = Frames()
        return self.play(self.media(path, ":no-audio"), frames), frames

    def record_voice(self, path: Path, voice: Path) -> Playback:
        return self.play(self.media(path, ":sout=" + VOICE_SOUT % voice.as_posix(), ":sout-avcodec-strict=-2", ":no-sout-video", ":no-sout-spu"))


def is_swapped(orientation: int) -> bool:
    return orientation & ORIENT_SWAP != 0


def check_media(player: Player, path: Path, media: Media, out_dir: Path) -> list[str]:
    failures = []
    parsed = player.parse(path)
    print(f"{media.name}: parsed with status {parsed.status}, duration {parsed.duration} ms, tracks {parsed.tracks}", flush=True)
    if not media.played:
        shown, frames = player.show_frames(path)
        print(f"{media.name}: {frames.count} frames, state {shown.state}, tracks {shown.tracks}", flush=True)
        return failures
    if parsed.status != PARSED_DONE:
        failures.append(f"{media.name}: parse status {parsed.status}")
    tracks = list(parsed.tracks)
    if media.video:
        shown, frames = player.show_frames(path)
        width, height = frames.size
        tracks += shown.tracks
        orientations = {track.orientation for track in shown.tracks if track.type == TRACK_VIDEO}
        print(f"{media.name}: {frames.count} frames of {width}x{height}, state {shown.state}, tracks {shown.tracks}", flush=True)
        if frames.count < FRAMES:
            failures.append(f"{media.name}: {frames.count} frames shown, expected {FRAMES}")
        if not orientations or any(is_swapped(orientation) != media.portrait for orientation in orientations):
            failures.append(f"{media.name}: video orientation {sorted(orientations)}")
        if frames.count and (height > width) != media.portrait:
            failures.append(f"{media.name}: frames of {width}x{height}, expected {'portrait' if media.portrait else 'landscape'}")
    voice = out_dir / f"{path.name}.m4a"
    recorded = player.record_voice(path, voice)
    tracks += recorded.tracks
    recording = player.parse(voice) if voice.exists() else Parsed(0, -1, [])
    print(f"{media.name}: voice recording state {recorded.state}, duration {recording.duration} ms, tracks {recording.tracks}", flush=True)
    if recording.tracks != [TrackInfo(TRACK_AUDIO, "mp4a", VOICE_CHANNELS, VOICE_RATE)] or recording.duration <= 0:
        failures.append(f"{media.name}: voice recording has duration {recording.duration} and tracks {recording.tracks}")
    types = {track.type for track in tracks}
    expected = {TRACK_AUDIO, TRACK_VIDEO} if media.video else {TRACK_AUDIO}
    if types != expected:
        failures.append(f"{media.name}: track types {sorted(types)}, expected {sorted(expected)}")
    return failures


def print_plugin_load_errors(vlc_dir: Path) -> None:
    for file in sorted((vlc_dir / "vlc" / "plugins").rglob("*_plugin.*")):
        if file.suffix in (".so", ".dll", ".dylib"):
            try:
                ctypes.CDLL(str(file))
            except OSError as error:
                print(f"cannot load {file.relative_to(vlc_dir)}: {error}", flush=True)


def check_played_audio(player: Player, path: Path) -> list[str]:
    played = player.play(player.media(path))
    print(f"{path.name}: state {played.state}, {played.decoded_audio} audio blocks decoded, {played.played_audio} played", flush=True)
    if played.state != STATE_ENDED or played.decoded_audio <= 0 or played.played_audio <= 0:
        return [f"{path.name}: state {played.state}, {played.decoded_audio} audio blocks decoded, {played.played_audio} played"]
    return []


def report(failures: list[str]) -> None:
    for failure in failures:
        print(f"FAIL {failure}", flush=True)


def start_player(vlc_dir: Path, args: tuple[str, ...]) -> Player:
    libvlc = load_libvlc(vlc_dir)
    print(f"libVLC {libvlc.libvlc_get_version().decode()}", flush=True)
    try:
        return Player(libvlc, args)
    except RuntimeError:
        print_plugin_load_errors(vlc_dir)
        raise


def check(vlc_dir: Path, media_dir: Path) -> list[str]:
    player = start_player(vlc_dir, PREVIEW_ARGS)
    failures = []
    with tempfile.TemporaryDirectory() as out:
        for media in GENERATED:
            failures += check_media(player, media_dir / media.name, media, Path(out))
        for sound in SOUNDS:
            failures += check_media(player, APP_SOUNDS / sound.name, sound, Path(out))
    failures += player.unexpected_modules()
    player.release()
    return failures


def check_audio(vlc_dir: Path, media_dir: Path) -> list[str]:
    player = start_player(vlc_dir, PLAYER_ARGS)
    failures = []
    for path in [media_dir / media.name for media in GENERATED if not media.video] + [APP_SOUNDS / sound.name for sound in SOUNDS]:
        failures += check_played_audio(player, path)
    failures += player.unexpected_modules()
    player.release()
    return failures


def run_checks(command: str, vlc_dir: Path, media_dir: Path) -> int:
    failures = check_bundle(vlc_dir)
    report(failures)
    played = check(vlc_dir, media_dir) if command == "check" else check_audio(vlc_dir, media_dir)
    report(played)
    print(f"{len(failures) + len(played)} failures", flush=True)
    return 1 if failures or played else 0


def main() -> int:
    parser = argparse.ArgumentParser(description="Test media and checks of the desktop VLC bundle")
    commands = parser.add_subparsers(dest="command", required=True)
    media = commands.add_parser("media", help="make the test media with FFmpeg")
    media.add_argument("dir", type=Path)
    for command, description in (
        ("check", "check the plugins of a VLC bundle, show the test media in frame callbacks and record their audio as voice messages"),
        ("audio", "check the plugins of a VLC bundle and play the audio test media and the call sounds on the system audio output"),
    ):
        bundle = commands.add_parser(command, help=description)
        bundle.add_argument("vlc_dir", type=Path)
        bundle.add_argument("media_dir", type=Path)
    args = parser.parse_args()
    if args.command == "media":
        make_media(args.dir)
        return 0
    return run_checks(args.command, args.vlc_dir.resolve(), args.media_dir.resolve())


if __name__ == "__main__":
    sys.exit(main())
