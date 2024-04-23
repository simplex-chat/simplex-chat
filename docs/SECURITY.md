# Security Policy

While great care is taken to ensure the highest level of security and privacy in SimpleX network servers and clients, all software can have flaws, and we believe it is a critical part of an organization's social responsibility to minimize the impact of these flaws through continual vulnerability discovery efforts, defense in depth design, and prompt remediation and notification.

The security assessment of SimpleX cryptography and networking was done by Trail of Bits in [November 2022](https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html).

We are planning design review of SimpleX protocols in July 2024 and implementation review in December 2024/January 2025.

## Reporting security issues

To report a security issue, please contact us directly via email [chat@simplex.chat](mailto:chat@simplex.chat). Please do NOT report security issues via GitHub issues or via any public channels.

Please encrypt the email message using the key for this address from [keys.openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat) (its fingerprint is `FB44 AF81 A45B DE32 7319 797C 8510 7E35 7D4A 17FC`) and make your key available for a secure reply.

While we encourage you to encrypt the message, if this poses a barrier to reporting, an unencrypted submission is better than no submission.

## Issue triage

Our team will investigate and prioritize the reported issue. We may work in private with individuals who are not our direct team members, as well as other organizations, where we believe this can help with the issue investigation, resolution, or testing.

## Threat Model

Please review threat model for SimpleX: https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md#threat-model

Certain threats are considered out of the scope of SimpleX security model. For example, we do not consider SimpleX secure against the following classes of attacks:

- CPU/hardware flaws.
- physical observation side channels (e.g. power consumption, EM emissions, etc).

Further, any user data stored on the device can be accessed with user's or root privileges, such as:
- user files in the app storage (encrypted or not, depending on the app settings).
- encrypted database.
- database encryption key in case it is stored on the device.

Mitigations for security issues outside of our threat model may still be implemented, however they will be weighed against competing priorities, and we do not classify them as SimpleX vulnerabilities.

## Issue severity

We will determine the risk of each issue, taking into account our experience dealing with past issues, versions affected, common defaults, and use cases. We classify issues on two dimensions, based on their severity level and the difficulty required to exploit them.

**Difficulty levels**

- **Low**: The flaw is well known; public tools for its exploitation exist or can be scripted.
- **Medium**: An attacker must write an exploit or will need in-depth knowledge of the system.
- **High**: An attacker must have privileged access to the system, may need to know complex technical details, or must discover other weaknesses to exploit this issue.

**Issue severity levels**

- **CRITICAL severity**. Such issues should affect common configurations and be exploitable with low or medium difficulty. For example: significant disclosure of the encrypted users messages or files either via relays or via communication channels, vulnerabilities which can be easily exploited remotely to compromise clients or servers private keys. These issues will be kept private and will trigger a new release of all supported versions.
- **HIGH severity**. This includes issues that are of a lower risk than critical, possibly due to affecting less common configurations, or have high difficulty to be exploited. These issues will be kept private and will trigger a new release of all supported versions.
- **MEDIUM severity**. This includes issues like crashes in client applications caused by the received messages or files, flaws in protocols that are less commonly used, and local flaws. These will in general be kept private until the next release, and that release will be scheduled so that it can roll up several such flaws at one time.
- **LOW severity**. This includes issues such as those that only affect the SimpleX CLI app, or unlikely configurations, or issues that would be classified as medium but are very difficult to exploit. These will in general be fixed immediately in latest development versions, and may be back-ported to older versions that are still getting updates. These issues may be kept private or be included in commit messages.

## Notification policy

Security fixes of critical, high and medium severity MUST NOT be mentioned in the commit message. Security fixes of low severity MAY be mentioned in the commit messages.

We will privately notify trusted partners about forthcoming security fixes on the day when the fix is publicly released and available for download via all supported channels, indicating the issue level, but not further details.

7 days after the new software version is released and available for download via all supported channels, we will indicate that it fixes a security issue and its level, but not further details. This notification will be published in our release notes and broadcast channels we use.

14 days later the details will be published in the release notes, describing the impact and the nature of vulnerability, but not necessarily providing detailed instruction for the exploit - it will be decided on a case by case basis.

## Trusted partners

You may be privately notified about the forthcoming releases containing fixes to the security issues of critical, high and medium severity. We will communicate only the level of the issue severity, and not the issue itself.

To be included in this list, you should be one of the following:
- software or hardware vendor depending on our code.
- commercial or non-profit organization that uses our software in scenarios where security and privacy is critically important.
- we may also include other organizations that are not listed but would otherwise qualify for list membership.
- we may also include organizations with which we have a commercial relationship.
- we may withdraw notifying certain organizations if they leak issues before they are public or do not add value.
