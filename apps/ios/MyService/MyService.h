//
//  MyService.h
//  MyService
//
//  Created by Evgeny on 01/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "MyServiceProtocol.h"

// This object implements the protocol which we have defined. It provides the actual behavior for the service. It is 'exported' by the service to make it available to the process hosting the service over an NSXPCConnection.
@interface MyService : NSObject <MyServiceProtocol>
@end
