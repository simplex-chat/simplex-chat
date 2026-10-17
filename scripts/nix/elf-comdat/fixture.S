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
.size comdat_function, .-comdat_function

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
