//
//  hs_init.c
//  SimpleXChat
//
//  Created by Evgeny on 22/11/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

#include "hs_init.h"

extern void hs_init_with_rtsopts(int * argc, char **argv[]);

void haskell_init(void) {
    int argc = 5;
    char *argv[] = {
        "simplex",
        "+RTS", // requires `hs_init_with_rtsopts`
        "-A16m", // chunk size for new allocations
        "-H64m", // initial heap size
        "-xn", // non-moving GC
        0
    };
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
}
