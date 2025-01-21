//
//  objc.h
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 09.09.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

#ifndef objc_h
#define objc_h

#import <Foundation/Foundation.h>

@interface ObjC : NSObject

+ (BOOL)catchException:(void(^)(void))tryBlock error:(__autoreleasing NSError **)error;

@end

#endif /* objc_h */
