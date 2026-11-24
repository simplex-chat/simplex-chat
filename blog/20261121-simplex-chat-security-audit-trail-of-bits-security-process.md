---
layout: layouts/article.html
title: "SimpleX Chat Security Audit by Trail of Bits and Our Security Process"
date: 2026-11-24
image: images/20221108-trail-of-bits.jpg
previewBody: blog_previews/20261124.html
permalink: "/blog/20261121-simplex-chat-security-audit-trail-of-bits-security-process.html"
---

# SimpleX Chat Security Audit by Trail of Bits and Our Security Process

**Published:** Nov 24, 2026

**Please upgrade to SimpleX Chat v7.1.4 or later, and ask your contacts to upgrade too.** It includes all fixes from this audit.

This release also includes supporter badges that allow sending larger files (2/5GB) available for longer (7/21 days) &mdash; buy the badge to <a href="https://badges.simplex.chat" target="_blank">support our work</a>.

SimpleX <a href="https://wefunder.com/simplex.chat?utm_source=blog" target="_blank">crowdfunding investors</a> get a free badge, and if you invest $500 or more, you will get a free SimpleX public name.

## The most comprehensive security audit of SimpleX so far

[Trail of Bits](https://www.trailofbits.com/about/) completed a comprehensive security audit of SimpleX Chat apps for iOS, Android and desktop, and of the simplexmq library that implements the protocols and servers of SimpleX network.

It is much larger than our previous security audits in [2022](./20221108-simplex-chat-v4.2-security-audit-new-website.md) and [2024](./20241014-simplex-network-v6-1-security-review-better-calls-user-experience.md). It covers 16 areas, from the post-quantum double ratchet and the messaging protocol to groups and channels, file transfer, calls, remote access from desktop, and storage of secrets on devices.

This depth was only possible because Trail of Bits engineers combined their expertise with frontier AI models: two engineer-weeks of review, with AI-assisted code analysis and formal modeling of the protocols.

## What we fixed, and when

Half of the high-severity issues were resolved before we received the final report &mdash; they were released in v7.0.0 in July. All other fixes were released in November.

Trail of Bits reviewed our fixes: 49 issues are marked as resolved and 12 as partially resolved. The remaining 14 are 2 issues we resolved after the fix review (released in v7.1.4), design decisions, part of the [security model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md#threat-model), or planned future work, such as including the parameters of the post-quantum key exchange in the security code.

The [addendum](./20261123-trail-of-bits-security-audit-2026-findings.md) lists all findings, where we resolved them, and our comments. The full report is available [here](https://github.com/simplex-chat/simplex-chat/blob/master/docs/SimpleX_Chat_Code_Review_2026_Final_Report_with_Fix_Review.pdf).

## Thank you, Trail of Bits

We are hugely thankful to Trail of Bits and its engineers for their hard work on this audit, and for the time they spent earlier in June helping us fix other security issues in SimpleX as part of the [Patch the Planet](https://trailofbits.com/patch-the-planet/) program.

## Our security process

We no longer see security as a periodic effort. It is an ongoing activity: we fix security issues as soon as we find them or they are reported, and release the fixes as soon as possible.

Not only do we fix issues in our code, we also analyse all dependencies as we upgrade them &mdash; we aim to complete the systematic review of all dependencies used by SimpleX Chat app and servers in the near future.

Anthropic's [OSS Scanner](https://red.anthropic.com/oss-scanner/) supports this process: it gives us access to security findings from Anthropic's frontier models that are not yet publicly available.

To report a security issue, please follow our [security policy](../docs/SECURITY.md).
