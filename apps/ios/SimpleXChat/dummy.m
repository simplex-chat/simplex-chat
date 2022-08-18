//
//  dummy.m
//  SimpleXChat
//
//  Created by Evgeny on 30/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#import <Foundation/Foundation.h>

#if defined(__x86_64__) && TARGET_IPHONE_SIMULATOR

#import <dirent.h>

int readdir_r$INODE64(DIR *restrict dirp, struct dirent *restrict entry,
                      struct dirent **restrict result) {
    return readdir_r(dirp, entry, result);
}

DIR *opendir$INODE64(const char *name) {
    return opendir(name);
}

#endif
