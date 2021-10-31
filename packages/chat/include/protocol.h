// Public API: Protocol.h

#ifndef protocol_h
#define protocol_h

#ifdef __cplusplus
extern "C" {  // only need to export C interface if
// used by C++ source code
#endif

const char* executeCommand(const char* command);

#ifdef __cplusplus
}
#endif

#endif /* protocol_h */
