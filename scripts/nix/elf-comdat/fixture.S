#if defined(NO_GROUP)
.text
#elif defined(PLAIN_GROUP)
.section .text.comdat_function,"axG",@progbits,GROUP_SIGNATURE
#else
.section .text.comdat_function,"axG",@progbits,GROUP_SIGNATURE,comdat
#endif
#if defined(WEAK_FUNCTION)
.weak comdat_function
#else
.globl comdat_function
#endif
.hidden comdat_function
.type comdat_function,@function
comdat_function:
    mov $PROBE_VALUE, %eax
    ret
#if defined(ZERO_SIZE_FUNCTION)
.size comdat_function, 0
#elif defined(PARTIAL_FUNCTION)
.size comdat_function, 1
#else
.size comdat_function, .-comdat_function
#endif

#if defined(EXTRA_FUNCTION)
.local PROBE_EXTRA
.type PROBE_EXTRA,@function
PROBE_EXTRA:
    ret
.size PROBE_EXTRA, .-PROBE_EXTRA
#elif defined(EXTRA_OBJECT)
.local PROBE_EXTRA
.type PROBE_EXTRA,@object
PROBE_EXTRA:
    .byte 0
.size PROBE_EXTRA, .-PROBE_EXTRA
#elif defined(EXTRA_GLOBAL)
.globl PROBE_EXTRA
.type PROBE_EXTRA,@notype
PROBE_EXTRA:
    nop
#elif defined(EXTRA_WEAK)
.weak PROBE_EXTRA
.type PROBE_EXTRA,@notype
PROBE_EXTRA:
    nop
#elif defined(EXTRA_LOCAL_LABEL)
.local PROBE_EXTRA
.type PROBE_EXTRA,@notype
PROBE_EXTRA:
    nop
#endif

#if defined(OTHER_SIGNATURE)
.globl other_signature
.set other_signature, comdat_function
#endif

.text
.globl PROBE_READER
.type PROBE_READER,@function
PROBE_READER:
    sub $8, %rsp
    call comdat_function
    add $8, %rsp
    add $PROBE_ADDEND, %eax
    ret
.size PROBE_READER, .-PROBE_READER

.section .note.GNU-stack,"",@progbits
