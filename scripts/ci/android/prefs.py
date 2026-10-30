#!/usr/bin/env python3

# Prints the app's SharedPreferences file with the settings given, typed as AppPreferences in SimpleXAPI.kt reads them.
# Usage: prefs.py <prefs .xml, empty when the app has none yet> <name>=<value>...

import sys
import xml.etree.ElementTree as ET

TYPES = {"OneHandUI": "boolean", "DeveloperTools": "boolean", "NetworkSMPProxyMode": "string", "LogLevel": "string", "AppearanceBarsBlurRadius": "int"}


def main():
    path, settings = sys.argv[1], sys.argv[2:]
    with open(path, "rb") as f:
        data = f.read().strip()
    prefs = ET.fromstring(data) if data else ET.Element("map")
    for setting in settings:
        name, _, value = setting.partition("=")
        kind = TYPES.get(name)
        if kind is None or (kind == "boolean" and value not in ("true", "false")) or (kind == "int" and not value.isdigit()):
            known = ", ".join(f"{n}=<{t}>" for n, t in TYPES.items())
            sys.exit(f"error: the settings are {known}, not {setting}")
        for old in prefs.findall(f"*[@name='{name}']"):
            prefs.remove(old)
        if kind in ("boolean", "int"):
            ET.SubElement(prefs, kind, name=name, value=value)
        else:
            ET.SubElement(prefs, kind, name=name).text = value
    print("<?xml version='1.0' encoding='utf-8' standalone='yes' ?>")
    print(ET.tostring(prefs, encoding="unicode"))


main()
