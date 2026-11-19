#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <linux/input-event-codes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <time.h>
#include <unistd.h>
#include <wayland-client.h>
#include <xkbcommon/xkbcommon.h>

#include "virtual-keyboard-unstable-v1-client-protocol.h"
#include "wlr-virtual-pointer-unstable-v1-client-protocol.h"

#define SCROLL_STEP 15

static struct wl_seat *seat;
static struct zwlr_virtual_pointer_manager_v1 *pointer_manager;
static struct zwp_virtual_keyboard_manager_v1 *keyboard_manager;

static void on_global(void *data, struct wl_registry *registry, uint32_t name, const char *interface, uint32_t version) {
    (void)data;
    (void)version;
    if (strcmp(interface, wl_seat_interface.name) == 0 && seat == NULL) {
        seat = wl_registry_bind(registry, name, &wl_seat_interface, 1);
    } else if (strcmp(interface, zwlr_virtual_pointer_manager_v1_interface.name) == 0) {
        pointer_manager = wl_registry_bind(registry, name, &zwlr_virtual_pointer_manager_v1_interface, 1);
    } else if (strcmp(interface, zwp_virtual_keyboard_manager_v1_interface.name) == 0) {
        keyboard_manager = wl_registry_bind(registry, name, &zwp_virtual_keyboard_manager_v1_interface, 1);
    }
}

static void on_global_remove(void *data, struct wl_registry *registry, uint32_t name) {
    (void)data;
    (void)registry;
    (void)name;
}

static const struct wl_registry_listener registry_listener = {on_global, on_global_remove};

static uint32_t now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint32_t)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
}

static int send_keymap(struct zwp_virtual_keyboard_v1 *keyboard) {
    struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
    struct xkb_keymap *keymap = context ? xkb_keymap_new_from_names(context, NULL, XKB_KEYMAP_COMPILE_NO_FLAGS) : NULL;
    char *text = keymap ? xkb_keymap_get_as_string(keymap, XKB_KEYMAP_FORMAT_TEXT_V1) : NULL;
    if (text == NULL) {
        fprintf(stderr, "wayland-input: no keymap\n");
        return -1;
    }
    size_t size = strlen(text) + 1;
    int fd = memfd_create("keymap", MFD_CLOEXEC);
    if (fd < 0 || write(fd, text, size) != (ssize_t)size) {
        fprintf(stderr, "wayland-input: keymap file: %s\n", strerror(errno));
        return -1;
    }
    zwp_virtual_keyboard_v1_keymap(keyboard, WL_KEYBOARD_KEYMAP_FORMAT_XKB_V1, fd, (uint32_t)size);
    close(fd);
    free(text);
    xkb_keymap_unref(keymap);
    xkb_context_unref(context);
    return 0;
}

static uint32_t linux_button(int button) {
    switch (button) {
        case 2: return BTN_MIDDLE;
        case 3: return BTN_RIGHT;
        default: return BTN_LEFT;
    }
}

static void run_command(struct zwlr_virtual_pointer_v1 *pointer, const char *line, uint32_t width, uint32_t height) {
    int x, y, button;
    char state[16];
    if (sscanf(line, "move %d %d", &x, &y) == 2) {
        zwlr_virtual_pointer_v1_motion_absolute(pointer, now_ms(), (uint32_t)x, (uint32_t)y, width, height);
    } else if (sscanf(line, "button %d %15s", &button, state) == 2) {
        uint32_t pressed = strcmp(state, "press") == 0 ? WL_POINTER_BUTTON_STATE_PRESSED : WL_POINTER_BUTTON_STATE_RELEASED;
        zwlr_virtual_pointer_v1_button(pointer, now_ms(), linux_button(button), pressed);
    } else if (sscanf(line, "scroll %15s", state) == 1) {
        int steps = strcmp(state, "up") == 0 ? -1 : 1;
        zwlr_virtual_pointer_v1_axis_source(pointer, WL_POINTER_AXIS_SOURCE_WHEEL);
        zwlr_virtual_pointer_v1_axis_discrete(pointer, now_ms(), WL_POINTER_AXIS_VERTICAL_SCROLL,
                                              wl_fixed_from_int(steps * SCROLL_STEP), steps);
    } else {
        fprintf(stderr, "wayland-input: unknown command: %s", line);
        return;
    }
    zwlr_virtual_pointer_v1_frame(pointer);
}

int main(int argc, char **argv) {
    if (argc != 4) {
        fprintf(stderr, "usage: wayland-input <fifo> <output width> <output height>\n");
        return 2;
    }
    uint32_t width = (uint32_t)atoi(argv[2]);
    uint32_t height = (uint32_t)atoi(argv[3]);
    struct wl_display *display = wl_display_connect(NULL);
    if (display == NULL) {
        fprintf(stderr, "wayland-input: no Wayland display\n");
        return 1;
    }
    struct wl_registry *registry = wl_display_get_registry(display);
    wl_registry_add_listener(registry, &registry_listener, NULL);
    wl_display_roundtrip(display);
    if (seat == NULL || pointer_manager == NULL || keyboard_manager == NULL) {
        fprintf(stderr, "wayland-input: the compositor lacks a seat or the virtual input protocols\n");
        return 1;
    }
    struct zwlr_virtual_pointer_v1 *pointer = zwlr_virtual_pointer_manager_v1_create_virtual_pointer(pointer_manager, seat);
    struct zwp_virtual_keyboard_v1 *keyboard = zwp_virtual_keyboard_manager_v1_create_virtual_keyboard(keyboard_manager, seat);
    if (send_keymap(keyboard) != 0) return 1;
    wl_display_roundtrip(display);
    FILE *commands = fopen(argv[1], "r+");
    if (commands == NULL) {
        fprintf(stderr, "wayland-input: %s: %s\n", argv[1], strerror(errno));
        return 1;
    }
    printf("ready\n");
    fflush(stdout);
    char line[256];
    while (fgets(line, sizeof(line), commands) != NULL) {
        run_command(pointer, line, width, height);
        if (wl_display_roundtrip(display) < 0) return 1;
    }
    return 0;
}
