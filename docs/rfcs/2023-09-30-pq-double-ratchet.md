# Post-quantum resistant augmented double ratchet algorithm

- [Overview](#overview)
- [Problem](#problem)
- [Comparison of the proposed solutions](#comparison-of-the-proposed-solutions)
  - [PQXDH for post-quantum key agreement](#pqxdh-for-post-quantum-key-agreement) (Signal)
  - [Hybrid Signal protocol for post-quantum encryption](#hybrid-signal-protocol-for-post-quantum-encryption) (Tutanota)
  - [Problems with ML-KEM / Kyber](#problems-with-ml-kem--kyber)
- [Proposed solution: augmented double ratchet algorithm](#proposed-solution-augmented-double-ratchet-algorithm)
- [Double ratchet with encrypted headers augmented with double PQ KEM](#double-ratchet-with-encrypted-headers-augmented-with-double-pq-kem)
  - [Initialization](#initialization)
  - [Encrypting messages](#encrypting-messages)
  - [Decrypting messages](#decrypting-messages)
- [Alternative approach: CTIDH](#alternative-approach-ctidh)
- [Implementation considerations for SimpleX Chat](#implementation-considerations-for-simplex-chat)
- [Summary](#summary)
- [Notes](#notes)

## 1. Overview

Currently SimpleX Chat uses [double-ratchet with header encryption](https://signal.org/docs/specifications/doubleratchet/#double-ratchet-with-header-encryption) to provide end-to-end encryption to messages and files. This document proposes a way to augment this algorithm with post-quantum key encapsulation mechanism (KEM) to make it resistant to quantum computers.

This document is purposefully written in an informal style to make it understandable for general audience with some technical, but without mathematical background. It does not compromise on the technical accuracy though.

## 2. Problem

It is a reasonable assumption that "record-now-decrypt-later" attacks are ongoing, so the users want to use cryptographic schemes for end-to-end encryption that are augmented with some post-quantum algorithm that is believed to be resistant to quantum computers.

Double-ratchet algorithm is a state of the art solution for end to end encryption offering a set of qualities that is unmatched by any other algorithm:

- perfect forward secrecy, i.e. compromise of session or long term keys does not lead to the ability to decrypt any of the past messages.
- deniability (also known as repudiation), i.e. the fact that the recipient of the message cannot prove to a third party that the sender actually sent this message [1].
- break-in recovery [2] (also know as post-compromise security or future secrecy), i.e. the ability of the end-to-end encryption security to recover from the compromise of the long term keys. This is achieved by generating a new random key pair whenever a new DH key is received (DH ratchet step).

It is desirable to preserve all these qualities when augmenting the algorithm with a post-quantum algorithm, and having these qualities resistant (or "believed to be" resistant [3]) to both conventional and quantum computers.

## Comparison of the proposed solutions

### PQXDH for post-quantum key agreement

[The solution](https://signal.org/docs/specifications/pqxdh/) recently [introduced by Signal](https://signal.org/blog/pqxdh/) augments the initial key agreement ([X3DH](https://signal.org/docs/specifications/x3dh/)) that is made prior to double ratchet algorithm. This is believed to provide protection from "record-now-decrypt-later" attack, but if the attacker at any point obtains long term keys from any of the devices, the break-in recovery will not be post-quantum resistant, and the attacker would be able to decrypt all the subsequent messages.

In addition to that, the authentication of parties in the proposed scheme is also not post-quantum resistant, although this is not part of double ratchet algorithm.

### Hybrid Signal protocol for post-quantum encryption

[The solution](https://eprint.iacr.org/2021/875.pdf) [proposed by Tutanota](https://tutanota.com/blog/posts/pqmail-update/) aims to preserve the break-in recovery property of double ratchet, but in doing so it:
- replaces rather than augments DH key agreement with post-quantum KEM mechanism, making it potentially vulnerable to conventional computers.
- adds signature to the DH ratchet step, to compensate for not keeping DH key agreement, but losing the deniability property for some of the messages.

### Problems with ML-KEM / Kyber

ML-KEM / Kyber used in both Signal and Tutanota schemes is the chosen algorithm by NIST, but its standardisation process raised some concerns amongst the experts:

- hashing of random numbers that was present in the original submission was removed from the standardised version of the algorithm. See lines 304-314 in the published spec (https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.203.ipd.pdf) and also the linked discussion on the subject: https://groups.google.com/a/list.nist.gov/g/pqc-forum/c/WFRDl8DqYQ4. This decision polarised the experts, with some of them saying that it effectively created a backdoor.
- calculation of security levels of Kyber appears to have been done incorrectly, and overall, the chosen Kyber seems worse than rejected NTRU according to [the analysis by Daniel Bernstein](https://blog.cr.yp.to/20231003-countcorrectly.html).

## Proposed solution: augmented double ratchet algorithm

We will augment the double ratchet algorithm with post-quantum KEM mechanism, preserving all properties of the double ratchet algorithm.

It is possible, because although double ratchet uses DH (which is a non-interactive key exchange [4]), it uses it "interactively", when the new DH keys are generated by both parties in turns. Parties of double-ratchet encrypted communication can run one or two post-quantum key encapsulation mechanisms in parallel with DH key agreement on each DH ratchet step, making break-in recovery of double ratchet algorithm post-quantum resistant (unlike Signal PQXDH scheme), without losing deniability or resistance to conventional computers (unlike Tutanota scheme).

Specifically, it is proposed to augment [double ratchet with encrypted headers](https://signal.org/docs/specifications/doubleratchet/#double-ratchet-with-header-encryption) with some post-quantum key encapsulation mechanism (KEM) as described below. A possible algorithm for PQ KEM is [NTRU-prime](https://ntruprime.cr.yp.to), that is currently adopted in SSH and has available implementations. It is important though that the proposed scheme can be used with any PQ KEM algorithm.

The downside of the proposed scheme is its substantial size overhead, as the encapsulation key and/or encapsulated shared secret are added to the header of each message. For the scheme described below, when both the encapsulation key and encapsulated secret are added to each message header, NTRU-prime adds ~2-4kb to each message (depending on the key size and the chosen variant). See [this table](https://ntruprime.cr.yp.to/security.html) for key and ciphertext sizes and the assessment of the security level for various key sizes.

## Double ratchet with encrypted headers augmented with double PQ KEM

Algorithm below assumes that in addition to shared secret from the initial key agreement, there will be an encapsulation key available from the party that published its keys (Bob).

### Initialization

The double ratchet initialization is defined in pseudocode as follows:

```
// Alice obtained Bob's keys and initializes ratchet first
def RatchetInitAlicePQ2HE(state, SK, bob_dh_public_key, shared_hka, shared_nhkb, bob_pq_kem_encapsulation_key):
    state.DHRs = GENERATE_DH()
    state.DHRr = bob_dh_public_key
    // below added for post-quantum KEM
    state.PQRs = GENERATE_PQKEM()
    state.PQRr = bob_pq_kem_encapsulation_key
    state.PQRss = random // shared secret for KEM
    state.PQRct = PQKEM-ENC(state.PQRr, state.PQRss) // encapsulated additional shared secret
    // above added for KEM
    // below augments DH key agreement with PQ shared secret
    state.RK, state.CKs, state.NHKs = KDF_RK_HE(SK, DH(state.DHRs, state.DHRr) || state.PQRss) 
    state.CKr = None
    state.Ns = 0
    state.Nr = 0
    state.PN = 0
    state.MKSKIPPED = {}
    state.HKs = shared_hka
    state.HKr = None
    state.NHKr = shared_nhkb

// Bob initializes ratchet second, having received Alice's connection request
def RatchetInitBobPQ2HE(state, SK, bob_dh_key_pair, shared_hka, shared_nhkb, bob_pq_kem_key_pair):
    state.DHRs = bob_dh_key_pair
    state.DHRr = None
    // below added for KEM
    state.PQRs = bob_pq_kem_key_pair
    state.PQRr = None
    state.PQRss = None
    state.PQRct = None
    // above added for KEM
    state.RK = SK 
    state.CKs = None
    state.CKr = None
    state.Ns = 0
    state.Nr = 0
    state.PN = 0
    state.MKSKIPPED = {}
    state.HKs = None
    state.NHKs = shared_nhkb
    state.HKr = None
    state.NHKr = shared_hka
```

`GENERATE_PQKEM` generates decapsulation/encapsulation key pair.

`PQKEM-ENC` is key encapsulation algorithm.

Other than commented lines, the above adds parameters `bob_pq_kem_encapsulation_key` and `bob_pq_kem_key_pair` to the ratchet intialization. Otherwise it is identical to the original double ratchet initialization.

### Encrypting messages

```
def RatchetEncryptPQ2HE(state, plaintext, AD):
    state.CKs, mk = KDF_CK(state.CKs)
    // encapsulation key from PQRs and encapsulated shared secret is added to header
    header = HEADER_PQ2(
      dh = state.DHRs.public,
      kem = state.PQRs.public, // added for KEM #2
      ct = state.PQRct // added for KEM #1
      pn = state.PN,
      n = state.Ns,
    )
    enc_header = HENCRYPT(state.HKs, header)
    state.Ns += 1
    return enc_header, ENCRYPT(mk, plaintext, CONCAT(AD, enc_header))
```

Other than adding encapsulation key and encapsulated shared secret into the header, the above is identical to the original double ratchet message encryption step.

As an optimization, to save space, it might be possible to add encapsulation key and encapsulated secret only when they change. The downside of this optimization would be that it will be impossible to decrypt the message when the message that has them is skipped or lost, compromising the ability of double ratchet to manage skipped messages.

### Decrypting messages

```
def RatchetDecryptPQ2HE(state, enc_header, ciphertext, AD):
    plaintext = TrySkippedMessageKeysHE(state, enc_header, ciphertext, AD)
    if plaintext != None:
        return plaintext
    header, dh_ratchet = DecryptHeader(state, enc_header) // DecryptHeader is the same as in double ratchet specification
    if dh_ratchet:
        SkipMessageKeysHE(state, header.pn) // SkipMessageKeysHE is the same as in double ratchet specification
        DHRatchetPQ2HE(state, header)
    SkipMessageKeysHE(state, header.n)
    state.CKr, mk = KDF_CK(state.CKr)
    state.Nr += 1
    return DECRYPT(mk, ciphertext, CONCAT(AD, enc_header))

// DecryptHeader is the same as in double ratchet specification
def DecryptHeader(state, enc_header):
    header = HDECRYPT(state.HKr, enc_header)
    if header != None:
        return header, False
    header = HDECRYPT(state.NHKr, enc_header)
    if header != None:
        return header, True
    raise Error()

def DHRatchetPQ2HE(state, header):
    state.PN = state.Ns
    state.Ns = 0
    state.Nr = 0
    state.HKs = state.NHKs
    state.HKr = state.NHKr
    state.DHRr = header.dh
    // save new encapsulation key from header
    state.PQRr = header.kem
    // decapsulate shared secret from header - KEM #2
    ss = PQKEM-DEC(state.PQRs.private, header.ct)
    // use decapsulated shared secret with receiving ratchet
    state.RK, state.CKr, state.NHKr = KDF_RK_HE(state.RK, DH(state.DHRs, state.DHRr) || ss)
    state.DHRs = GENERATE_DH()
    // below is added for KEM
    state.PQRs = GENERATE_PQKEM() // generate new PQ key pair
    state.PQRss = random // shared secret for KEM
    state.PQRct = PQKEM-ENC(state.PQRr, state.PQRss) // encapsulated additional shared secret KEM #1
    // above is added for KEM
    // use new shared secret with sending ratchet
    state.RK, state.CKs, state.NHKs = KDF_RK_HE(state.RK, DH(state.DHRs, state.DHRr) || state.PQRss)
```

`PQKEM-DEC` is key decapsulation algorithm.

`DHRatchetPQ2HE` augments both DH agreements with decapsulated shared secret from the received header and with the new shared secret, respectively. The new shared secret together with the new encapsulation key are saved in the state and will be added to the header in the next sent message.

Other than augmenting DH key agreements with the shared secrets from KEM, the above is identical to the original double ratchet DH ratchet step.

It is worth noting that while DH agreements work as ping-pong, when the new received DH key is used for both DH agreements (and only the sent DH key is updated for the second DH key agreement), PQ KEM agreements in the proposed scheme work as a "parallel ping-pong", with two balls in play all the time (two KEM agreements run in parallel).

## Alternative approach: CTIDH

Instead of using KEM, we can consider using [CTIDH](https://ctidh.isogeny.org). The advantage is a smaller key size, and also as CTIDH is non-interactive [4], there is no need to run two key agreements in parallel, the PQ keys would simply augment DH keys and would be used in the same way.

The main downside is the absense of performance-efficient implementation for aarch64 architecture.

## Implementation considerations for SimpleX Chat

As SimpleX Chat pads messages to a fixed size, using 16kb transport blocks, the size increase introduced by this scheme will not cause additional traffic in most cases. For large texts it may require additional messages to be sent. Similarly, for media previews it may require either reducing the preview size (and quality), or sending additional messages, or compressing the current JSON encoding, e.g. with zstd algorithm.

That might be the primary reason why this scheme was not adopted by Signal, as it would have resulted in substantial traffic growth – to the best of our knowledge, Signal messages are not padded to a fixed size.

Sharing the initial keys in case of SimpleX Chat it is equivalent to sharing the invitation link. As encapsulation key is large, it may be inconvenient to share it in the link in some contexts.

It is possible to postpone sharing the encapsulation key until the first message from Alice (confirmation message in SMP protocol), the party sending connection request. The upside here is that the invitation link size would not increase. The downside is that the user profile shared in this confirmation will not be encrypted with PQ-resistant algorithm. To mitigate it, the hadnshake protocol can be modified to postpone sending the user profile until the second message from Alice (HELLO message in SMP protocol).

Another consideration is pairwise ratchets in groups. Key generation in sntrup761 is quite slow - on slow devices it can probably be as slow as 10 keys per second, so using this primitive in groups larger than 10 members would result in slow performance. An option could be not to use ratchets in groups at all, but that would result in the lack of protection in small groups that simply combine multiple devices of 1-3 people. So a better option would be to support dynamically adding and removing sntrup761 keys for pairwise ratchets in groups, which means that when sending each message a boolean flag needs to be passed whether to use PQ KEM or not.

## Summary

If chosen PQ KEM proves secure against quantum computer attacks, then the proposed augmented double ratchet will also be secure against quantum computer attack, including break-in recovery property, while keeping deniability and forward secrecy, because the [same proof](https://eprint.iacr.org/2016/1013.pdf) as for double ratchet algorithm would hold here, provided KEM is secure.

## Notes

[1] This is often misunderstood to mean that the recipient cannot prove that the sender sent the message at all, which is incorrect, as the recipient has the proof that either themselves or the sender encrypted the message, and as they know that the recipient themselves did not encrypt it, therefore the sender did. So the communication is secure and authenticated for the parties, without providing a proof to a third party.

[2] The term "break-in recovery" is used in this document, consistent with the terminology of the double ratchet algorithm specification, and also because it can be used to mean both the quality of being able to recover from the compromise and the actual process used to recover.

[3] This is important to remember that no existing post-quantum algorithms are proven to be resistant to quantum or even conventional computers, therefore the correct approach is to augment rather than replace existing algorithms with the post-quantum ones.

[4] Non-interactive key exchange is a type of key agreement that allows both parties to generate key pairs independently, without input from another parties, and then use the public keys from each other to compute the same shared secret.
