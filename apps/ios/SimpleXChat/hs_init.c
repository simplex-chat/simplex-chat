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
        "-A64m", // chunk size for new allocations
        "-H64m", // initial heap size
        "-xn", // non-moving GC
        0
    };
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
}

void haskell_init_nse(void) {
    int argc = 9;
    char *argv[] = {
        "simplex",
        "+RTS", // requires `hs_init_with_rtsopts`
        "-A1m", // chunk size for new allocations
        "-H1m", // initial heap size
        "-M12m", // maximum heap size (25 total - 1 grace - 12 for swift), make GC work harder when approaching the limit
        "-Mgrace=1m", // (default, just to make explicit) extra memory to handle "memory exhausted" exception and fail cleanly
        "-F0.5", // heap growth triggering GC
        "-Fd1", // memory return
        "-c", // compacting garbage collector
        0
    };
    char **pargv = argv;
    hs_init_with_rtsopts(&argc, &pargv);
}
