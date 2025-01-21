//
//  objc.m
//  SimpleXChat
//
//  Created by Stanislav Dmitrenko on 09.09.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

#import "objc.h"

@implementation ObjC

// https://stackoverflow.com/a/36454808
+ (BOOL)catchException:(void(^)(void))tryBlock error:(__autoreleasing NSError **)error {
    @try {
        tryBlock();
        return YES;
    }
    @catch (NSException *exception) {
        *error = [[NSError alloc] initWithDomain: exception.name code: 0 userInfo: exception.userInfo];
        return NO;
    }
}

@end
