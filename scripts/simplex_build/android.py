"""The Android APKs, Play bundle and core libraries, and their reproduction."""

import contextlib
import enum
import os
import shutil
import tempfile
import zipfile
from collections.abc import Iterator, Mapping, Sequence
from dataclasses import dataclass, field
from pathlib import Path

from .apk_zip import apk_padding, copy_v1_flags
from .common import (
    SIMPLEX_REPO,
    BuildError,
    checkout,
    download,
    extract_flat,
    log,
    release_url,
    run,
    single_file,
    warn,
)
from .docker import OUT, PROJECT, Container, Step, retry_once
from .nix import NIX_CACHE_CONFIG, install_nix, nix_env


class Arch(enum.StrEnum):
    AARCH64 = "aarch64"
    ARMV7A = "armv7a"

    @property
    def abi(self) -> str:
        return {Arch.AARCH64: "arm64-v8a", Arch.ARMV7A: "armeabi-v7a"}[self]


# SHA-256 of the certificate that signs the released APKs
SIMPLEX_KEY = "3C:52:C4:FD:3C:AD:1C:07:C9:B0:0A:70:80:E3:58:FA:B9:FE:FC:B8:AF:5A:EC:14:77:65:F1:6D:0F:21:AD:85"


ANDROID_LIBS_DIR = "apps/multiplatform/common/src/commonMain/cpp/android/libs"


ANDROID_LIBS = ("libsimplex", "libsupport")


# The flake.nix jobs of the Android core libraries
ANDROID_LIB_JOBS = {"libsimplex": "lib:simplex-chat", "libsupport": "lib:support"}


ANDROID_BUNDLE_DIR = "apps/multiplatform/android/build/outputs/bundle/googleRelease"


# The release key that android/build.gradle.kts signs with: the keystore's path and its passwords and alias
ANDROID_SIGNING_ENV = ("ANDROID_KEYSTORE", "ANDROID_KEYSTORE_PASSWORD", "ANDROID_KEY_ALIAS", "ANDROID_KEY_PASSWORD")


ANDROID_KEYSTORE_MOUNT = "/keystore/release.keystore"


# The local.properties of release-mobile.py's builds
ANDROID_LOCAL_PROPERTIES = {
    "compression.level": "5",
    "simplex.assets.dir": "../../assets",
}


def android_build_tag(tag: str, existing_tags: Sequence[str], arch: Arch) -> str:
    """Releases before armv7a was built from master have it in a separate <tag>-armv7a tag."""
    if arch == Arch.ARMV7A and f"{tag}-armv7a" in existing_tags:
        return f"{tag}-armv7a"
    return tag


def android_vercode(source: Path) -> str:
    for line in (source / "apps/multiplatform/gradle.properties").read_text().splitlines():
        if line.startswith("android.version_code="):
            return line.split("=", 1)[1].strip()
    raise BuildError("android.version_code not found in apps/multiplatform/gradle.properties")


@dataclass(frozen=True, slots=True)
class AndroidSigning:
    keystore: Path
    store_password: str = field(repr=False)
    key_alias: str
    key_password: str = field(repr=False)

    @staticmethod
    def from_env(environ: Mapping[str, str]) -> "AndroidSigning | None":
        """The release key of the ANDROID_SIGNING_ENV variables, or None when none of them is set."""
        values = [environ.get(name, "") for name in ANDROID_SIGNING_ENV]
        if not any(values):
            return None
        missing = [name for name, value in zip(ANDROID_SIGNING_ENV, values, strict=True) if not value]
        if missing:
            raise BuildError(f"set all of {', '.join(ANDROID_SIGNING_ENV)} to sign, or none; missing: {', '.join(missing)}")
        keystore = Path(values[0])
        if not keystore.is_file():
            raise BuildError(f"the keystore {keystore} is not found")
        return AndroidSigning(keystore.resolve(), values[1], values[2], values[3])

    @property
    def env(self) -> dict[str, str]:
        return {"ANDROID_KEYSTORE": ANDROID_KEYSTORE_MOUNT, "ANDROID_KEY_ALIAS": self.key_alias}

    @property
    def secrets(self) -> dict[str, str]:
        return {"ANDROID_KEYSTORE_PASSWORD": self.store_password, "ANDROID_KEY_PASSWORD": self.key_password}


def android_signing() -> AndroidSigning | None:
    signing = AndroidSigning.from_env(os.environ)
    if signing is None:
        warn(f"{', '.join(ANDROID_SIGNING_ENV)} are not set: building unsigned")
    return signing


@contextlib.contextmanager
def android_container(source: Path, out: Path, set_pid_max: bool, signing: AndroidSigning | None = None) -> Iterator[Container]:
    mounts = {source.resolve(): PROJECT, out.resolve(): OUT}
    if signing:
        mounts[signing.keystore] = f"{ANDROID_KEYSTORE_MOUNT}:ro"
    # FUSE and SYS_ADMIN are what Dockerfile.build's builds need; systempaths=unconfined makes /proc/sys writable
    with Container(
        source,
        mounts,
        run_args=[
            "--device", "/dev/fuse",
            "--cap-add", "SYS_ADMIN",
            "--security-opt", "apparmor:unconfined",
            "--security-opt", "seccomp:unconfined",
            "--security-opt", "systempaths=unconfined",
        ],
    ) as container:
        if set_pid_max:
            # 32-bit bionic aborts above pid 65535, which the armv7a GHC runs under qemu. Since Linux 6.14 pid_max
            # belongs to each pid namespace, copied when it is created, so it is set in the container's.
            container.run(Step(("tee", "/proc/sys/kernel/pid_max"), user="root", input="65535\n"))
        yield container


def gradle_path(container: Container) -> str:
    """PATH with the gradle of the project's wrapper first, which build-android.sh needs as `gradle`."""
    container.run(Step(("./gradlew", "--version"), workdir=f"{PROJECT}/apps/multiplatform"))
    home = container.run(Step(("printenv", "HOME")), capture=True).strip()
    found = container.run(
        Step(("find", f"{home}/.gradle/wrapper/dists", "-name", "gradle", "-type", "f", "-executable")), capture=True
    ).split()
    if not found:
        raise BuildError("the gradle wrapper's distribution is not found")
    path = container.run(Step(("printenv", "PATH")), capture=True).strip()
    return f"{Path(found[0]).parent}:{path}"


def build_apk(
    container: Container,
    source: Path,
    arch: Arch,
    out: Path,
    out_name: str,
    nix_cache: bool,
    keep_libs: bool,
    signing: AndroidSigning | None = None,
) -> None:
    """The deterministic APK of scripts/android/build-android.sh, signed by Gradle with signing, as <out>/<out_name>."""
    env = {"ARCHES": arch.value, "PATH": gradle_path(container)}
    if nix_cache:
        env["NIX_CONFIG"] = NIX_CACHE_CONFIG
    secrets = None
    if signing:
        env |= signing.env
        secrets = signing.secrets
    build = Step(("./scripts/android/build-android.sh", "-gs", android_vercode(source)), env=env)
    retry_once(lambda: container.run(build, secrets=secrets))
    shutil.copy(source / f"simplex-chat-{arch.abi}.apk", out / out_name)
    if keep_libs:
        for lib in ANDROID_LIBS:
            container.copy_out(f"{PROJECT}/result-{arch}-{lib}/pkg-{arch}-android-{lib}.zip", out)


def build_bundle(container: Container, source: Path, libs: Path, out: Path, signing: AndroidSigning | None = None) -> None:
    # Gradle reads compression.level only from local.properties; a non-zero level extracts the native libraries at
    # install, as the APKs do.
    properties = "".join(f"{key}={value}\n" for key, value in ANDROID_LOCAL_PROPERTIES.items())
    (source / "apps/multiplatform/local.properties").write_text(properties)
    for arch in Arch:
        target = source / ANDROID_LIBS_DIR / arch.abi
        target.mkdir(parents=True, exist_ok=True)
        for lib in ANDROID_LIBS:
            name = f"pkg-{arch}-android-{lib}.zip"
            found = sorted(libs.rglob(name))
            if not found:
                raise BuildError(f"{name} not found in {libs}")
            with zipfile.ZipFile(found[0]) as z:
                z.extractall(target)
    build = Step(
        ("./gradlew", "--no-daemon", ":android:bundleGoogleRelease"),
        workdir=f"{PROJECT}/apps/multiplatform",
        env=signing.env if signing else {},
    )
    retry_once(lambda: container.run(build, secrets=signing.secrets if signing else None))
    shutil.copy(single_file(source / ANDROID_BUNDLE_DIR, "*.aab"), out / "simplex.aab")


def build_android_libs(source: Path, arch: Arch, out: Path, install: bool) -> None:
    """The core libraries of the APK, built with Nix on this machine, as the emulator tests use them."""
    with tempfile.TemporaryDirectory() as tmp:
        work = Path(tmp)
        if install:
            install_nix(work)
        for lib, job in ANDROID_LIB_JOBS.items():
            result = work / f"result-{lib}"
            run(["nix", "build", "-L", "--out-link", result, f".#hydraJobs.x86_64-linux.{arch}-android:{job}"], cwd=source, env=nix_env())
            shutil.copy(result / f"pkg-{arch}-android-{lib}.zip", out)


def install_android_libs(source: Path, arch: Arch, packages: Path) -> None:
    for lib in ANDROID_LIBS:
        extract_flat(packages / f"pkg-{arch}-android-{lib}.zip", source / ANDROID_LIBS_DIR / arch.abi)


def release_signer(container: Container, apk_name: str) -> str:
    output = container.run(Step(("apksigner", "verify", "--print-certs", f"{OUT}/{apk_name}")), capture=True)
    digest = next(line.split()[-1] for line in output.splitlines() if "SHA-256" in line)
    return ":".join(digest[i : i + 2] for i in range(0, len(digest), 2)).upper()


def copy_signature(container: Container, out: Path, released: str, unsigned: str) -> bytes:
    """The unsigned APK with the released APK's signature, and the v1 signature flags that apksigcopier drops."""
    container.run(Step(("apksigcopier", "copy", f"{OUT}/{released}", f"{OUT}/{unsigned}", f"{OUT}/{unsigned}.signed")))
    signed = out / f"{unsigned}.signed"
    signed.write_bytes(copy_v1_flags((out / released).read_bytes(), signed.read_bytes()))
    return signed.read_bytes()


def verify_apk(container: Container, out: Path, released: str, built: str) -> bool:
    """Copies the released APK's signature onto the built one and compares them: onto the APK as built, as AGP signs
    releases, then onto it aligned and padded as apksigner signed the releases before."""
    released_data = (out / released).read_bytes()
    if copy_signature(container, out, released, built) == released_data:
        return True
    pad = ("--pad-like-apksigner",) if apk_padding(released_data) == "apksigner" else ()
    container.run(Step(("repro-apk", "zipalign", "--page-size", "16", *pad, "--replace", f"{OUT}/{built}", f"{OUT}/{built}.aligned")))
    return copy_signature(container, out, released, f"{built}.aligned") == released_data


def reproduce_android(tag: str, repo: str, arches: Sequence[Arch], nix_cache: bool, source: Path, results: Path) -> list[str]:
    out = results / "android"
    out.mkdir(parents=True, exist_ok=True)
    tags = run(["git", "-C", source, "tag", "--list"], capture=True).stdout.split()
    for arch in arches:
        download(release_url(tag, f"simplex-{arch}.apk", repo), out / f"simplex-{arch}.apk")
    failed = []
    with android_container(source, out, Arch.ARMV7A in arches) as container:
        for arch in arches:
            released = f"simplex-{arch}.apk"
            signer = release_signer(container, released)
            if repo != SIMPLEX_REPO:
                warn(f"{released} of {repo} is signed by {signer}, which is not checked")
            elif signer != SIMPLEX_KEY:
                raise BuildError(f"{released} is signed by {signer}, not by the SimpleX key")
            else:
                log(f"{released} is signed by the SimpleX key")
            checkout(source, android_build_tag(tag, tags, arch))
            log(f"building {arch}")
            build_apk(container, source, arch, out, f"{released}.built", nix_cache, keep_libs=False)
            if verify_apk(container, out, released, f"{released}.built"):
                log(f"{released} is reproduced")
            else:
                log(f"{released} is NOT reproduced")
                failed.append(released)
    return failed
