#include <jni.h>
#include <gtk/gtk.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TAO_RENDERER_ENV "NUCLEUS_TAO_LINUX_RENDERER"
#define X11_BACKEND "x11"
#define GNOME_INTERFACE_SCHEMA "org.gnome.desktop.interface"
#define GTK_THEME_KEY "gtk-theme"

static JavaVM *g_vm = NULL;

typedef struct {
    jobject callback;
} chooser_request;

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *vm, void *reserved) {
    (void)reserved;
    g_vm = vm;
    return JNI_VERSION_1_8;
}

static jobjectArray paths_to_java(JNIEnv *env, GSList *files) {
    jclass byte_array_class = (*env)->FindClass(env, "[B");
    if (byte_array_class == NULL) return NULL;
    jobjectArray result = (*env)->NewObjectArray(env, (jsize)g_slist_length(files), byte_array_class, NULL);
    if (result == NULL) return NULL;
    jsize index = 0;
    for (GSList *file = files; file != NULL; file = file->next, index++) {
        const char *path = file->data;
        jsize length = (jsize)strlen(path);
        jbyteArray bytes = (*env)->NewByteArray(env, length);
        if (bytes == NULL) return NULL;
        (*env)->SetByteArrayRegion(env, bytes, 0, length, (const jbyte *)path);
        (*env)->SetObjectArrayElement(env, result, index, bytes);
        (*env)->DeleteLocalRef(env, bytes);
    }
    return result;
}

static void deliver(chooser_request *request, GSList *files) {
    JNIEnv *env = NULL;
    if ((*g_vm)->GetEnv(g_vm, (void **)&env, JNI_VERSION_1_8) != JNI_OK) {
        (*g_vm)->AttachCurrentThread(g_vm, (void **)&env, NULL);
    }
    jobjectArray paths = files != NULL ? paths_to_java(env, files) : NULL;
    jclass callback_class = (*env)->GetObjectClass(env, request->callback);
    jmethodID on_result = (*env)->GetMethodID(env, callback_class, "onResult", "([[B)V");
    if (on_result != NULL) {
        (*env)->CallVoidMethod(env, request->callback, on_result, paths);
    }
    if ((*env)->ExceptionCheck(env)) {
        (*env)->ExceptionDescribe(env);
        (*env)->ExceptionClear(env);
    }
    (*env)->DeleteGlobalRef(env, request->callback);
    free(request);
}

static void on_response(GtkNativeDialog *dialog, gint response, gpointer data) {
    GSList *files = response == GTK_RESPONSE_ACCEPT ? gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(dialog)) : NULL;
    deliver(data, files);
    g_slist_free_full(files, g_free);
    g_object_unref(dialog);
}

static char *java_to_utf8(JNIEnv *env, jstring value) {
    if (value == NULL) return NULL;
    const char *chars = (*env)->GetStringUTFChars(env, value, NULL);
    if (chars == NULL) return NULL;
    char *copy = g_strdup(chars);
    (*env)->ReleaseStringUTFChars(env, value, chars);
    return copy;
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_platform_LinuxNative_nativeChooseFiles(
    JNIEnv *env, jclass clazz, jlong parent, jstring title, jint action, jboolean multiple,
    jstring current_name, jstring filter_name, jobjectArray patterns, jobject callback)
{
    (void)clazz;
    char *title_utf8 = java_to_utf8(env, title);
    GtkFileChooserNative *dialog = gtk_file_chooser_native_new(
        title_utf8, parent != 0 ? GTK_WINDOW((gpointer)(intptr_t)parent) : NULL,
        (GtkFileChooserAction)action, NULL, NULL);
    g_free(title_utf8);
    GtkFileChooser *chooser = GTK_FILE_CHOOSER(dialog);
    gtk_file_chooser_set_select_multiple(chooser, multiple == JNI_TRUE);
    if (action == GTK_FILE_CHOOSER_ACTION_SAVE) {
        gtk_file_chooser_set_do_overwrite_confirmation(chooser, TRUE);
    }
    char *name_utf8 = java_to_utf8(env, current_name);
    if (name_utf8 != NULL) {
        gtk_file_chooser_set_current_name(chooser, name_utf8);
        g_free(name_utf8);
    }
    if (patterns != NULL) {
        GtkFileFilter *filter = gtk_file_filter_new();
        char *filter_name_utf8 = java_to_utf8(env, filter_name);
        if (filter_name_utf8 != NULL) {
            gtk_file_filter_set_name(filter, filter_name_utf8);
            g_free(filter_name_utf8);
        }
        jsize count = (*env)->GetArrayLength(env, patterns);
        for (jsize i = 0; i < count; i++) {
            jstring pattern = (jstring)(*env)->GetObjectArrayElement(env, patterns, i);
            char *pattern_utf8 = java_to_utf8(env, pattern);
            if (pattern_utf8 != NULL) {
                gtk_file_filter_add_pattern(filter, pattern_utf8);
                g_free(pattern_utf8);
            }
            (*env)->DeleteLocalRef(env, pattern);
        }
        gtk_file_chooser_add_filter(chooser, filter);
    }
    gtk_native_dialog_set_modal(GTK_NATIVE_DIALOG(dialog), TRUE);
    chooser_request *request = malloc(sizeof(chooser_request));
    if (request == NULL) {
        g_object_unref(dialog);
        return;
    }
    request->callback = (*env)->NewGlobalRef(env, callback);
    g_signal_connect(dialog, "response", G_CALLBACK(on_response), request);
    gtk_native_dialog_show(GTK_NATIVE_DIALOG(dialog));
}

JNIEXPORT jstring JNICALL
Java_chat_simplex_common_platform_LinuxNative_nativeGtkThemeName(JNIEnv *env, jclass clazz)
{
    (void)clazz;
    GSettingsSchemaSource *source = g_settings_schema_source_get_default();
    if (source == NULL) return NULL;
    GSettingsSchema *schema = g_settings_schema_source_lookup(source, GNOME_INTERFACE_SCHEMA, TRUE);
    if (schema == NULL) return NULL;
    jboolean has_key = g_settings_schema_has_key(schema, GTK_THEME_KEY) ? JNI_TRUE : JNI_FALSE;
    g_settings_schema_unref(schema);
    if (has_key == JNI_FALSE) return NULL;
    GSettings *settings = g_settings_new(GNOME_INTERFACE_SCHEMA);
    gchar *theme = g_settings_get_string(settings, GTK_THEME_KEY);
    g_object_unref(settings);
    jstring result = (*env)->NewStringUTF(env, theme);
    g_free(theme);
    return result;
}

JNIEXPORT jboolean JNICALL
Java_chat_simplex_common_platform_LinuxNative_nativeConfirm(
    JNIEnv *env, jclass clazz, jstring title, jstring message)
{
    (void)clazz;
    const char *renderer = getenv(TAO_RENDERER_ENV);
    if (renderer != NULL && strcmp(renderer, X11_BACKEND) == 0) {
        gdk_set_allowed_backends(X11_BACKEND);
    }
    if (!gtk_init_check(NULL, NULL)) return JNI_FALSE;
    char *title_utf8 = java_to_utf8(env, title);
    char *message_utf8 = java_to_utf8(env, message);
    GtkWidget *dialog = gtk_message_dialog_new(
        NULL, GTK_DIALOG_MODAL, GTK_MESSAGE_WARNING, GTK_BUTTONS_YES_NO, "%s", message_utf8 != NULL ? message_utf8 : "");
    gtk_window_set_title(GTK_WINDOW(dialog), title_utf8 != NULL ? title_utf8 : "");
    g_free(title_utf8);
    g_free(message_utf8);
    gint response = gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    while (gtk_events_pending()) gtk_main_iteration();
    return response == GTK_RESPONSE_YES ? JNI_TRUE : JNI_FALSE;
}
