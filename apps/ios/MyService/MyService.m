//
//  MyService.m
//  MyService
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#import "MyService.h"

@implementation MyService

// This implements the example protocol. Replace the body of this class with the implementation of this service's protocol.
- (void)upperCaseString:(NSString *)aString withReply:(void (^)(NSString *))reply {
    NSString *response = [aString uppercaseString];
    reply(response);
}

@end
