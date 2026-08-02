#import <AppKit/AppKit.h>
#import <UserNotifications/UserNotifications.h>
#include <jni.h>

static NSString * const SimpleXVibrancyIdentifier = @"chat.simplex.sidebar.vibrancy";
static NSString * const SimpleXMessageCategory = @"MESSAGE";
static NSString * const SimpleXContactCategory = @"CONTACT_REQUEST";
static NSString * const SimpleXCallCategory = @"INCOMING_CALL";
static JavaVM *SimpleXJavaVM = NULL;
static jclass SimpleXCallbackClass = NULL;
static jmethodID SimpleXCallbackMethod = NULL;

@interface SimpleXNotificationDelegate : NSObject <UNUserNotificationCenterDelegate>
@end

@implementation SimpleXNotificationDelegate
- (void)userNotificationCenter:(UNUserNotificationCenter *)center
       willPresentNotification:(UNNotification *)notification
         withCompletionHandler:(void (^)(UNNotificationPresentationOptions options))completionHandler {
    completionHandler(UNNotificationPresentationOptionBanner |
                      UNNotificationPresentationOptionList |
                      UNNotificationPresentationOptionSound);
}

- (void)userNotificationCenter:(UNUserNotificationCenter *)center
 didReceiveNotificationResponse:(UNNotificationResponse *)response
          withCompletionHandler:(void (^)(void))completionHandler {
    NSDictionary *info = response.notification.request.content.userInfo;
    if (SimpleXJavaVM != NULL && SimpleXCallbackClass != NULL && SimpleXCallbackMethod != NULL) {
        JNIEnv *env = NULL;
        BOOL detach = NO;
        jint status = (*SimpleXJavaVM)->GetEnv(SimpleXJavaVM, (void **)&env, JNI_VERSION_1_6);
        if (status == JNI_EDETACHED && (*SimpleXJavaVM)->AttachCurrentThread(SimpleXJavaVM, (void **)&env, NULL) == JNI_OK) detach = YES;
        if (env != NULL) {
            NSString *chatId = info[@"chatId"] ?: @"";
            NSString *action = response.actionIdentifier ?: UNNotificationDefaultActionIdentifier;
            jstring jChatId = (*env)->NewStringUTF(env, chatId.UTF8String);
            jstring jAction = (*env)->NewStringUTF(env, action.UTF8String);
            (*env)->CallStaticVoidMethod(
                env,
                SimpleXCallbackClass,
                SimpleXCallbackMethod,
                [info[@"userId"] longLongValue],
                [info[@"remoteHostId"] longLongValue],
                jChatId,
                [info[@"messageId"] longLongValue],
                jAction
            );
            (*env)->DeleteLocalRef(env, jChatId);
            (*env)->DeleteLocalRef(env, jAction);
            if ((*env)->ExceptionCheck(env)) {
                (*env)->ExceptionDescribe(env);
                (*env)->ExceptionClear(env);
            }
        }
        if (detach) (*SimpleXJavaVM)->DetachCurrentThread(SimpleXJavaVM);
    }
    completionHandler();
}
@end

static SimpleXNotificationDelegate *SimpleXNotificationDelegateInstance = nil;

static NSString *stringFromJNI(JNIEnv *env, jstring value) {
    if (value == NULL) return nil;
    const char *chars = (*env)->GetStringUTFChars(env, value, NULL);
    if (chars == NULL) return nil;
    NSString *result = [NSString stringWithUTF8String:chars];
    (*env)->ReleaseStringUTFChars(env, value, chars);
    return result;
}

static NSSet<UNNotificationCategory *> *notificationCategories(void) {
    UNNotificationAction *acceptContact = [UNNotificationAction actionWithIdentifier:@"ACCEPT_CONTACT_REQUEST" title:@"Accept" options:UNNotificationActionOptionForeground];
    UNNotificationAction *acceptCall = [UNNotificationAction actionWithIdentifier:@"ACCEPT_CALL" title:@"Accept" options:UNNotificationActionOptionForeground];
    UNNotificationAction *rejectCall = [UNNotificationAction actionWithIdentifier:@"REJECT_CALL" title:@"Decline" options:UNNotificationActionOptionDestructive];
    return [NSSet setWithArray:@[
        [UNNotificationCategory categoryWithIdentifier:SimpleXMessageCategory actions:@[] intentIdentifiers:@[] options:UNNotificationCategoryOptionNone],
        [UNNotificationCategory categoryWithIdentifier:SimpleXContactCategory actions:@[acceptContact] intentIdentifiers:@[] options:UNNotificationCategoryOptionNone],
        [UNNotificationCategory categoryWithIdentifier:SimpleXCallCategory actions:@[acceptCall, rejectCall] intentIdentifiers:@[] options:UNNotificationCategoryOptionNone],
    ]];
}

static BOOL configureWindow(NSWindow *window) {
    if (window == nil || window.contentView == nil) return NO;

    window.styleMask |= NSWindowStyleMaskFullSizeContentView;
    window.titlebarAppearsTransparent = YES;
    window.titleVisibility = NSWindowTitleHidden;

    NSView *contentView = window.contentView;
    for (NSView *view in contentView.subviews.copy) {
        if ([view.identifier isEqualToString:SimpleXVibrancyIdentifier]) return YES;
    }

    NSVisualEffectView *effectView = [[NSVisualEffectView alloc] initWithFrame:contentView.bounds];
    effectView.identifier = SimpleXVibrancyIdentifier;
    effectView.material = NSVisualEffectMaterialSidebar;
    effectView.blendingMode = NSVisualEffectBlendingModeBehindWindow;
    effectView.state = NSVisualEffectStateFollowsWindowActiveState;
    effectView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
    [contentView addSubview:effectView positioned:NSWindowBelow relativeTo:contentView.subviews.firstObject];
    return YES;
}

JNIEXPORT jboolean JNICALL
Java_chat_simplex_common_MacOSPlatformKt_macOSConfigureWindow(JNIEnv *env, jclass clazz, jlong windowHandle) {
    if (windowHandle == 0) return JNI_FALSE;
    __block BOOL configured = NO;
    void (^work)(void) = ^{
        @autoreleasepool {
            configured = configureWindow((__bridge NSWindow *)(void *)windowHandle);
        }
    };
    if ([NSThread isMainThread]) work(); else dispatch_sync(dispatch_get_main_queue(), work);
    return configured ? JNI_TRUE : JNI_FALSE;
}

JNIEXPORT jboolean JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSInitializeNotifications(JNIEnv *env, jclass clazz) {
    if ((*env)->GetJavaVM(env, &SimpleXJavaVM) != JNI_OK) return JNI_FALSE;
    if (SimpleXCallbackClass == NULL) {
        jclass localClass = (*env)->FindClass(env, "chat/simplex/common/model/MacOSNotificationsKt");
        if (localClass == NULL) return JNI_FALSE;
        SimpleXCallbackClass = (*env)->NewGlobalRef(env, localClass);
        (*env)->DeleteLocalRef(env, localClass);
        SimpleXCallbackMethod = (*env)->GetStaticMethodID(env, SimpleXCallbackClass, "onMacOSNotificationResponse", "(JJLjava/lang/String;JLjava/lang/String;)V");
        if (SimpleXCallbackMethod == NULL) return JNI_FALSE;
    }
    void (^work)(void) = ^{
        if (SimpleXNotificationDelegateInstance == nil) SimpleXNotificationDelegateInstance = [SimpleXNotificationDelegate new];
        UNUserNotificationCenter *center = UNUserNotificationCenter.currentNotificationCenter;
        center.delegate = SimpleXNotificationDelegateInstance;
        [center setNotificationCategories:notificationCategories()];
    };
    if ([NSThread isMainThread]) work(); else dispatch_sync(dispatch_get_main_queue(), work);
    return JNI_TRUE;
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSRequestNotificationPermission(JNIEnv *env, jclass clazz) {
    [UNUserNotificationCenter.currentNotificationCenter requestAuthorizationWithOptions:(UNAuthorizationOptionAlert | UNAuthorizationOptionSound | UNAuthorizationOptionBadge)
                                                                      completionHandler:^(BOOL granted, NSError *error) {}];
}

JNIEXPORT jint JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSNotificationPermissionState(JNIEnv *env, jclass clazz) {
    __block UNAuthorizationStatus status = UNAuthorizationStatusNotDetermined;
    dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
    [UNUserNotificationCenter.currentNotificationCenter getNotificationSettingsWithCompletionHandler:^(UNNotificationSettings *settings) {
        status = settings.authorizationStatus;
        dispatch_semaphore_signal(semaphore);
    }];
    dispatch_semaphore_wait(semaphore, dispatch_time(DISPATCH_TIME_NOW, (int64_t)(2 * NSEC_PER_SEC)));
    switch (status) {
        case UNAuthorizationStatusNotDetermined: return 0;
        case UNAuthorizationStatusDenied: return 1;
        case UNAuthorizationStatusAuthorized: return 2;
        case UNAuthorizationStatusProvisional: return 3;
        default: return 4;
    }
}

JNIEXPORT jboolean JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSDeliverNotification(
    JNIEnv *env, jclass clazz, jstring identifier, jlong userId, jlong remoteHostId, jstring chatId,
    jlong messageId, jstring title, jstring body, jstring category, jboolean playSound, jstring imagePath
) {
    UNMutableNotificationContent *content = [UNMutableNotificationContent new];
    NSString *identifierValue = stringFromJNI(env, identifier);
    NSString *chatIdValue = stringFromJNI(env, chatId);
    content.title = stringFromJNI(env, title) ?: @"SimpleX";
    content.body = stringFromJNI(env, body) ?: @"";
    content.categoryIdentifier = stringFromJNI(env, category) ?: SimpleXMessageCategory;
    content.threadIdentifier = [NSString stringWithFormat:@"%lld:%lld:%@", (long long)userId, (long long)remoteHostId, chatIdValue];
    content.userInfo = @{
        @"userId": @(userId),
        @"remoteHostId": @(remoteHostId),
        @"chatId": chatIdValue ?: @"",
        @"messageId": @(messageId),
    };
    if (playSound) content.sound = UNNotificationSound.defaultSound;
    NSString *path = stringFromJNI(env, imagePath);
    if (path.length > 0) {
        UNNotificationAttachment *attachment = [UNNotificationAttachment attachmentWithIdentifier:@"image" URL:[NSURL fileURLWithPath:path] options:nil error:nil];
        if (attachment != nil) content.attachments = @[attachment];
    }
    UNNotificationRequest *request = [UNNotificationRequest requestWithIdentifier:identifierValue content:content trigger:nil];
    [UNUserNotificationCenter.currentNotificationCenter addNotificationRequest:request withCompletionHandler:^(NSError *error) {}];
    return JNI_TRUE;
}

static void removeNotificationsMatching(BOOL (^matches)(NSDictionary *info)) {
    UNUserNotificationCenter *center = UNUserNotificationCenter.currentNotificationCenter;
    [center getDeliveredNotificationsWithCompletionHandler:^(NSArray<UNNotification *> *notifications) {
        NSMutableArray<NSString *> *identifiers = [NSMutableArray array];
        for (UNNotification *notification in notifications) {
            if (matches(notification.request.content.userInfo)) [identifiers addObject:notification.request.identifier];
        }
        if (identifiers.count > 0) [center removeDeliveredNotificationsWithIdentifiers:identifiers];
    }];
    [center getPendingNotificationRequestsWithCompletionHandler:^(NSArray<UNNotificationRequest *> *requests) {
        NSMutableArray<NSString *> *identifiers = [NSMutableArray array];
        for (UNNotificationRequest *request in requests) {
            if (matches(request.content.userInfo)) [identifiers addObject:request.identifier];
        }
        if (identifiers.count > 0) [center removePendingNotificationRequestsWithIdentifiers:identifiers];
    }];
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSRemoveNotificationsForChat(JNIEnv *env, jclass clazz, jlong userId, jlong remoteHostId, jstring chatId) {
    NSString *chatIdValue = stringFromJNI(env, chatId);
    removeNotificationsMatching(^BOOL(NSDictionary *info) {
        return [info[@"userId"] longLongValue] == userId && [info[@"remoteHostId"] longLongValue] == remoteHostId && [info[@"chatId"] isEqualToString:chatIdValue];
    });
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSRemoveNotificationsForUser(JNIEnv *env, jclass clazz, jlong userId) {
    removeNotificationsMatching(^BOOL(NSDictionary *info) { return [info[@"userId"] longLongValue] == userId; });
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSRemoveAllNotifications(JNIEnv *env, jclass clazz) {
    [UNUserNotificationCenter.currentNotificationCenter removeAllDeliveredNotifications];
    [UNUserNotificationCenter.currentNotificationCenter removeAllPendingNotificationRequests];
}

JNIEXPORT void JNICALL
Java_chat_simplex_common_model_MacOSNotificationsKt_macOSOpenNotificationSettings(JNIEnv *env, jclass clazz) {
    NSURL *url = [NSURL URLWithString:@"x-apple.systempreferences:com.apple.Notifications-Settings.extension"];
    if (url != nil) dispatch_async(dispatch_get_main_queue(), ^{ [NSWorkspace.sharedWorkspace openURL:url]; });
}
