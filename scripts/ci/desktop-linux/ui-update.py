#!/usr/bin/env python3

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import zipfile
from pathlib import Path

OWN_CLASSES = ("chat/simplex/desktop/MainKt.class", "chat/simplex/common/BuildConfigCommon.class")
JAR_CONTENT_HASH = re.compile(r"(-[0-9a-f]{16,32})?\.jar$")


def own(jar: Path) -> bool:
    with zipfile.ZipFile(jar) as z:
        return any(name in OWN_CLASSES for name in z.namelist())


def base(app: Path) -> str:
    jars = app / "lib/app"
    core_hash = (jars / "resources/core-hash").read_text().strip()
    cfg = next(jars.glob("*.cfg")).read_text().splitlines()
    names = [line.rsplit("/", 1)[-1] for line in cfg if line.startswith("app.classpath=")]
    dependencies = sorted(JAR_CONTENT_HASH.sub("", name) for name in names if not own(jars / name))
    return hashlib.sha256("".join(f"{line}\n" for line in [core_hash, *dependencies]).encode()).hexdigest()


def copy_jar(jar: Path, target: Path, without: str | None) -> None:
    if without is None:
        shutil.copy(jar, target)
        return
    with zipfile.ZipFile(jar) as source, zipfile.ZipFile(target, "w", zipfile.ZIP_DEFLATED) as copy:
        for entry in source.infolist():
            if entry.filename != without:
                copy.writestr(entry, source.read(entry))


def main() -> None:
    parser = argparse.ArgumentParser(description="Stages the app's own jars as a UI update, as AppUpdater.kt stages one it downloads.")
    parser.add_argument("app", type=Path, help="the installed app, with lib/app/resources/core-hash")
    parser.add_argument("jars", type=Path, help="the app's own jars of the update")
    parser.add_argument("key", type=Path, help="the Ed25519 private key, PEM, whose public key the app was built with")
    parser.add_argument("version_code", type=int)
    parser.add_argument("out", type=Path, help="the update's directory in the app's ui_updates")
    parser.add_argument("--without", help="a jar entry left out of the update, such as the main class")
    args = parser.parse_args()
    args.out.mkdir(parents=True)
    for jar in sorted(args.jars.glob("*.jar")):
        copy_jar(jar, args.out / jar.name, args.without)
    jars = {jar.name: hashlib.sha256(jar.read_bytes()).hexdigest() for jar in sorted(args.out.glob("*.jar"))}
    manifest = args.out / "ui.json"
    manifest.write_text(json.dumps({"version_code": args.version_code, "base": base(args.app), "jars": jars}))
    sign = ["openssl", "pkeyutl", "-sign", "-inkey", args.key, "-rawin", "-in", manifest, "-out", args.out / "ui.json.sig"]
    subprocess.run(sign, check=True)


if __name__ == "__main__":
    main()
