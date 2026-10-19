/* SPDX-License-Identifier: AGPL-3.0-only */
/* Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd. */

/* Bionic's ARM static TLS layout needs 32-byte alignment. API 26's crtbegin
 * predates that input section; retain this zero-sized alignment constraint
 * even when linking the standalone regression with --gc-sections.
 */
.section .tdata.simplex_android_tls_align,"awTR",%progbits
.p2align 5
.section .note.GNU-stack,"",%progbits
