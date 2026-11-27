#!/usr/bin/env python3

import hashlib
import importlib.util
import json
import re
import shutil
import sys
import tempfile
import zipfile
from pathlib import Path

spec = importlib.util.spec_from_file_location("ui_update", Path(__file__).parents[1] / "desktop-linux/ui-update.py")
ui_update = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ui_update)


def main(jars: Path, runtime: Path, out: Path) -> None:
    properties = Path("apps/multiplatform/gradle.properties").read_text()
    version_code = int(re.search(r"^desktop\.version_code=(\d+)$", properties, re.MULTILINE)[1])
    with tempfile.TemporaryDirectory() as tmp:
        app = Path(tmp)
        shutil.copytree(jars, app / "lib/app")
        (app / "lib/runtime").mkdir()
        shutil.copy(runtime, app / "lib/runtime/release")
        base = ui_update.base(app)
    own = [jar for jar in sorted(jars.glob("*.jar")) if ui_update.own(jar)]
    manifest = {"version_code": version_code, "base": base, "jars": {jar.name: hashlib.sha256(jar.read_bytes()).hexdigest() for jar in own}}
    out.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(out / f"simplex-desktop-ui-{base}.zip", "w", zipfile.ZIP_DEFLATED) as z:
        z.writestr("ui.json", json.dumps(manifest))
        for jar in own:
            z.write(jar, jar.name)
    print(f"simplex-desktop-ui-{base}.zip: {', '.join(jar.name for jar in own)}")


if __name__ == "__main__":
    main(*map(Path, sys.argv[1:4]))
