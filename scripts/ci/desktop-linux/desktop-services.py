#!/usr/bin/env python3

import json
import sys

import dbus
import dbus.mainloop.glib
import dbus.service
from gi.repository import GLib

WATCHER_NAME = "org.kde.StatusNotifierWatcher"
WATCHER_PATH = "/StatusNotifierWatcher"
NOTIFICATIONS_NAME = "org.freedesktop.Notifications"
NOTIFICATIONS_PATH = "/org/freedesktop/Notifications"
NOTIFICATIONS_TEST = "chat.simplex.ci.Notifications"
LAUNCHER_ENTRY = "com.canonical.Unity.LauncherEntry"
PROPERTIES = "org.freedesktop.DBus.Properties"
CAPABILITIES = ["body", "body-markup", "actions", "action-icons", "persistence"]

log_file = open(sys.argv[1], "a", buffering=1)


def record(event, **fields):
    log_file.write(json.dumps({"event": event, **fields}) + "\n")


class Watcher(dbus.service.Object):
    def __init__(self, bus):
        super().__init__(bus, WATCHER_PATH)
        self.items = []

    @dbus.service.method(WATCHER_NAME, in_signature="s", sender_keyword="sender")
    def RegisterStatusNotifierItem(self, service, sender=None):
        item = f"{sender}{service}" if service.startswith("/") else str(service)
        self.items.append(item)
        record("tray_registered", service=str(service), sender=str(sender))
        self.StatusNotifierItemRegistered(item)

    @dbus.service.method(WATCHER_NAME, in_signature="s")
    def RegisterStatusNotifierHost(self, service):
        record("host_registered", service=str(service))

    @dbus.service.signal(WATCHER_NAME, signature="s")
    def StatusNotifierItemRegistered(self, item):
        pass

    @dbus.service.method(PROPERTIES, in_signature="ss", out_signature="v")
    def Get(self, interface, name):
        return self.GetAll(interface)[name]

    @dbus.service.method(PROPERTIES, in_signature="s", out_signature="a{sv}")
    def GetAll(self, interface):
        return {
            "RegisteredStatusNotifierItems": dbus.Array(self.items, signature="s"),
            "IsStatusNotifierHostRegistered": dbus.Boolean(True),
            "ProtocolVersion": dbus.Int32(0),
        }


class Notifications(dbus.service.Object):
    def __init__(self, bus):
        super().__init__(bus, NOTIFICATIONS_PATH)
        self.next_id = 1

    @dbus.service.method(NOTIFICATIONS_NAME, out_signature="as")
    def GetCapabilities(self):
        return CAPABILITIES

    @dbus.service.method(NOTIFICATIONS_NAME, out_signature="ssss")
    def GetServerInformation(self):
        return "desktop-services", "SimpleX CI", "1", "1.2"

    @dbus.service.method(NOTIFICATIONS_NAME, in_signature="susssasa{sv}i", out_signature="u")
    def Notify(self, app_name, replaces_id, app_icon, summary, body, actions, hints, timeout):
        notification_id = replaces_id or self.next_id
        if not replaces_id:
            self.next_id += 1
        record(
            "notify",
            id=int(notification_id),
            replaces_id=int(replaces_id),
            app_name=str(app_name),
            summary=str(summary),
            body=str(body),
            actions=[str(a) for a in actions],
            hints=sorted(str(k) for k in hints.keys()),
            desktop_entry=str(hints.get("desktop-entry", "")),
            category=str(hints.get("category", "")),
        )
        return notification_id

    @dbus.service.method(NOTIFICATIONS_NAME, in_signature="u")
    def CloseNotification(self, notification_id):
        record("close", id=int(notification_id))
        self.NotificationClosed(notification_id, 3)

    @dbus.service.signal(NOTIFICATIONS_NAME, signature="uu")
    def NotificationClosed(self, notification_id, reason):
        pass

    @dbus.service.signal(NOTIFICATIONS_NAME, signature="us")
    def ActionInvoked(self, notification_id, action_key):
        pass

    @dbus.service.method(NOTIFICATIONS_TEST, in_signature="us")
    def Invoke(self, notification_id, action_key):
        record("invoke", id=int(notification_id), action=str(action_key))
        self.ActionInvoked(notification_id, action_key)


def launcher_update(uri, properties):
    record("launcher", uri=str(uri), urgent=bool(properties.get("urgent", False)))


dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
session = dbus.SessionBus()
session.add_signal_receiver(launcher_update, signal_name="Update", dbus_interface=LAUNCHER_ENTRY)
names = [dbus.service.BusName(WATCHER_NAME, session), dbus.service.BusName(NOTIFICATIONS_NAME, session)]
services = [Watcher(session), Notifications(session)]
record("ready")
GLib.MainLoop().run()
