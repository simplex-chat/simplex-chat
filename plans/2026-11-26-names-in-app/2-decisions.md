# SimpleX names in the apps: decisions

The owner's decisions that bind this plan, for the agents who implement and review it. Each entry: the decision, its date, its place, the owner's words, and the rules of part 1 it shapes. "Item" is the message number in the owner's conversation with the agents. Decisions made for the names page or the store service are entered as made; part 1 states where its rules apply them to the app. A claim about the owner's intent without the owner's words or commit is kept in the questions, not here.

The earlier work is material: spaced4ndy's and spaced4ndy-agents' pull requests, brenzi's #7530, brenzi-agent's #8026, and brenzi's wallet and in-app wallet branches (#7355, #7382 and the `ab/wallet*` branches). The owner's wallet #7684 is the base of the plan (D41). Where the pictures of that material answer a question, the answer is entered in the last section, D42 to D52, with its picture as source. brenzi's in-app wallet departs from D5, D12, D20 and D21, which stand.

## Scope

### D1. Names are bought and registered inside the apps
- Date: 2026-11-26
- Place: conversation, item 15944
- Words: "propose a high level plan about how to bring names purchases into the app."
- Shapes: R4, R5, R8

### D2. In-app payments
- Date: 2026-11-18
- Place: conversation, store service review, item 5406
- Words: "we would obv be adding in-app payments."
- Shapes: R4, R8

### D3. Names are sold as badges are sold
- Decision: selling names in the apps follows the badge purchase in the apps; nothing new is invented; small badge improvements may follow, or an audit.
- Date: 2026-11-26
- Place: conversation
- Words: "how we sell badges - it's structurally the same approach, so please do NOT invent new things, and maybe we can improve some small things in badges too, or at least audit."
- Shapes: R4, R5, R8, R13

### D4. The names branch went to master without the device wallet
- Decision: the names feature branch was prepared for master without the wallet added to the app.
- Date: 2026-11-14
- Place: conversation, items 43 and 95; commit c194aefb9 on `names`, by the owner: "Revert "core: wallet for names (#7684)""
- Words: "we need to urgently prepare names branch in simplex-chat repo to master, but without any changes for wallet that was added to the app" (item 43); "so can it be just cherry-pick to say names-wallet + revert in names?" (item 95)
- Followed by: D41, the apps build on the same wallet, restored on current master.
- Shapes: D41

### D41 (Q1). The apps build on the #7684 device wallet
- Decision: names bought in the apps are controlled by the device wallet of #7684: one 24-word phrase per device, and one account of it for each name. The wallet is restored on current master as the branch `ep/names-wallet`: master and a revert of c194aefb9. The plan contains the UX of the wallet.
- Date: 2026-11-26
- Place: SimpleX channel, items 16289, 16294, 16302, 16312 and 16318
- Words: "when writing the plan, have you seen wallet branch we will be building on top of?" (item 16289); "it's probably behind, and needs master merge into it..." (item 16294); "look at app-names branch" (item 16302)
- Words: "correct, this is it, ok." (item 16312), the answer to the team lead's proposal: "The quickest base with it, on current master, is a new branch `ep/names-wallet`: master plus a revert of `c194aefb9`. That brings the wallet back exactly as it was merged, with conflicts in the 7 files I listed. Branching from the PR's head would mean merging 400+ master commits instead. Should I make that branch and use it as the plan's base?"
- Words: "I think the plan should contain UX - does it not?" (item 16318)
- Shapes: R1, R5, R6, R10, R11, R15, R16, R17, R18; flows 1 to 6

## Price

### D5. $100 for 2 years, any length
- Decision: every name costs $100 for 2 years at launch; the model keeps prices by length; tests keep different prices by length.
- Date: 2026-11-22
- Place: conversation, store service review, item 10088
- Words: "fyi, we launch with $100 all. But let's keep different prices in tests just in case we want to change it"
- Shapes: R3

## Codes

### D6. Free names for investors, registered with a free name code
- Date: 2026-11-05 and 2026-11-22
- Place: conversation (2026-11-05); conversation, item 9391 (2026-11-22)
- Words: "investors get free names" (2026-11-05); "1) select name 2) pay or provide free name code" (item 9391)
- Shapes: R2

### D7. A free name code has no monetary value
- Decision: no text, API, document or plan says or implies that a code pays or is a payment method.
- Date: 2026-11-20
- Place: conversation, names page review, item 6770
- Words: "Code can be used to register, it cannot be used to pay for anything, and it certainly cannot 'pay'"
- Shapes: R2, R12

### D8. An accepted free name code stays usable for a month after a take
- Decision: past its own expiry, for one calendar month after the name is taken by another order; a revoked code is refused.
- Date: 2026-11-22
- Place: conversation, #8243, item 9757
- Words: "We have say 1 month to use it once accepted, same as we have 1 month to register a name. Never is also wrong, it can be abused"
- Shapes: R6

## Orders

### D9. One order, one name, one key
- Date: 2026-11-21
- Place: conversation, #8243, item 8115
- Words: "The key is the name, as simple as that. Order can only have one name. Key can only have one name"
- Shapes: R1, R6

### D10. A take resets the order's name; nothing moves
- Decision: the order is reset to "name not selected" and keeps its payment or code; selecting a name again is an ordinary order; unspent money is spent on one name or refunded.
- Date: 2026-11-21
- Place: conversation, #8243, item 8120
- Words: "Unspent balance can only be spent on one name, period … Any unspent balance left after buying a name can only be refunded"
- Shapes: R6

### D11. Selection and payment in either order; the reset needs no request
- Date: 2026-11-22
- Place: conversation, items 9391 and 9395
- Words: "The order is irrelevant and instead of doing "move" we just reset selection, and ask to re-select, with the rest of the order staying the same - it's either paid or with code." (item 9391)
- Words: "reset happens at a point service finds out that selected name is taken, so no separate API call is needed for reset. It further simplifies state machine" (item 9395)
- Shapes: R6

### D12. One request places the order with everything signed
- Decision: the service receives the commitment, the signed registration and the terms acceptance with the order, before the invoice; registration then continues without the buyer online; a further signature is needed only in rare cases.
- Date: 2026-11-18
- Place: conversation, store service review, item 5680
- Words: "Service receives both commitment AND signed transaction before issuing invoice. In this case further registration no longer depends on user being online. It can happen as soon as payment completes. So the user simply: 1) sends one request (terms acceptance, commitment, signed transaction included) 2) receives invoice 3) pays 4) polls the service to monitor registration status."
- Shapes: R5

### D13. The service learns the names of unpaid orders
- Decision: accepted as the cost of D12; it may be disclosed.
- Date: 2026-11-18
- Place: conversation, item 5689
- Words: "so the only threat model degradation here would be that service learns about names of unpaid orders. We may explicitely disclose it"
- Shapes: D43

### D14. Operators hear of unregistered paid orders within 3 hours; an order waiting for its owner fails after 3 days unread
- Date: 2026-11-18
- Place: conversation, store service review, item 5318
- Words: "Agreed? 3 hours to alert, 3 days to fail."
- Shapes: R5

### D15. Two co-dependent state machines
- Decision: a design that touches orders draws the client's and the service's state machines together, with their links.
- Date: 2026-11-25
- Place: conversation, item 14534
- Words: "we have 2 co-dependent state machines here, and we never looked at the second"
- Shapes: part 1, section 6

### D16. The order and its name code are made at once
- Date: 2026-11-25
- Place: conversation, item 14465
- Words: "there is no reason not to create current order and name code at once - again, the state machine completes unnecessary states it seems"
- Shapes: R1

## Search and availability

### D17. Names the service does not sell show as available
- Decision: such names show as available at search, and their orders are refused.
- Date: 2026-11-26
- Place: conversation, item 15169; #8349
- Words: "show them as available at search always and refuse registration at order stage."
- Shapes: R7

## Texts

### D19. Only people act
- Date: 2026-11-20
- Place: conversation, item 6654
- Words: "there are some exceptions to this rule, and we sometimes use active words with objects, like "the plane flies", but it's never done with abstract concepts."
- Shapes: R12

### D20. Legal texts use no ownership language
- Date: 2026-11-10
- Place: conversation, on the names conditions
- Words: ""Your" implies ownership, and given that we are getting opinion that it's not value or obligation, ownership implies value. Correct word is "controls"."
- Shapes: R12

### D21. The names page's texts are approved
- Decision: the page's texts, as reviewed on 2026-11-26, are approved; part 1 uses the same text for the same situation in the app.
- Date: 2026-11-26
- Place: conversation, item 15910
- Words: "ok, all approved, execute"
- Shapes: R12

### D22. A title says what the person can do
- Date: 2026-11-19
- Place: conversation, names page review, item 6094
- Words: "title "Paid, not registered" - this is not good. Is commitment phase also shows this title? also, if no action is required from the user why we need to say "the name was not signed in time"? what user can do with this information"
- Shapes: R12

### D23. "No longer available" covers every reason
- Date: 2026-11-26
- Place: conversation, item 15768
- Words: "> was registered by someone else first / is no longer available / This would cover a wider class of reasons I'd say."
- Shapes: R7, R12

## The name code and the name address

### D24. The name address beside the name code
- Decision: wherever the name code is shown, the name address is shown too; support requests include the name address and the order reference, never the name code.
- Date: 2026-11-24
- Place: conversation, names page hardening, items 13697 and 13820
- Words: "the page should also give customer their wallet address too (we will call it "name address"), so when they contact support they quote both wallet address and the order referenece" (item 13697); ""You need this code to control your name and edit the links. Never share it with us or anybody." "Your name address is public. Send it with any support requests."" (item 13820)
- Shapes: R11

### D25. A name code entered where a free name code is asked
- Date: 2026-11-20
- Place: conversation, names page review, item 6618
- Words: "User should be told that it's a name code, and if it has links, and offered to add to the page into "Your names""
- Shapes: flow 4

### D26. The name on an independent explorer
- Date: 2026-11-24
- Place: conversation, names page hardening, item 13651
- Words: "yes, link to visible nft on etherscan, with request to check that they can see it there, and contact support otherwise"
- Shapes: the names page only; the app follows D53

## Values the client holds, and signed requests

### D28. The page holds the deployment's values
- Decision: the contracts, the chain, the terms, the prices, the dispute days and the payment keys are held in the page and change only with a reviewed page release.
- Date: 2026-11-24
- Place: conversation, names page hardening, item 13105; `plans/2026-11-25-names-hardening.md` on `names-2` (#8343)
- Words: "yes. It also means that updating any of these would require a small maintenance window."
- Shapes: the names page; the app follows D62

### D29. Every request about an order is signed with its name key
- Date: 2026-11-24
- Place: conversation, names page hardening, item 13877
- Words: "correct. Every request to order ID must be authenticated by name key, that is - signed"
- Shapes: R10

### D30. Any paid price off the held list is refused
- Date: 2026-11-24
- Place: conversation, names page hardening, item 13105
- Words: "yes, any price. Service gets prices from the contract, so rollout is simpler."
- Shapes: R9

### D40. A name code still opens its order after a change of the resolver address
- Decision: the resolver address of a deployment can change; a name code added after such a change must still open its order. The fix of the names page is a separate pull request.
- Date: 2026-11-26
- Place: conversation, item 15902, the answer to: "Can the resolver address change for a deployment? If it can, screen 2 also needs a code fix in a separate PR. If it can't, the text alone is enough."
- Words: "yes, agreed"
- Shapes: R14

## Terms

### D31 (Q14). The names terms are conditions of use, apart from purchase terms
- Date: 2026-11-02
- Place: the owner's answer on the names conditions proposal, question q110, choice "at checkout"
- Words: "Yes, the terms should be focussed on usage, they are like conditions of use, decoupled from purchase terms."
- Shapes: purchase terms linked at checkout

## Store service data

### D32. Names use the generic tables and their own, never the badges' tables
- Date: 2026-11-18
- Place: conversation, store service review, item 5406
- Words: "Names product MUST use generic tables (can't just use its own) and MUST NOT use badge tables, it MUST use its own."
- Shapes: part 3

### D33. The live store database holds badge data only
- Date: 2026-11-22
- Place: conversation, item 10331
- Words: "No, live database holds no data in names, only badges. We can't lose badges data. We have no names data in released code."
- Shapes: part 3

## Badges

### D34. Names work leaves badge-only code unchanged
- Date: 2026-11-22
- Place: conversation, #8243, item 10774
- Words: "changes affecting ONLY badges must be reverted in this PR, this PR was about names. The rest should be very carefully reviewed."
- Shapes: Q19, part 3

### D35. iOS store purchases in builds the App Store serves
- Decision: badge purchases with the App Store are shown in builds the App Store serves; TestFlight, App Review and Xcode builds offer the badge page and a code.
- Date: 2026-11-05
- Place: #8044, approved by the owner's review and merged by the owner (https://github.com/simplex-chat/simplex-chat/pull/8044)
- Shapes: R8

## Process

### D36. User stories first
- Date: 2026-11-25
- Place: conversation on #8073, as quoted in the decision record supplied with this task
- Words: "One stage we are missing, consistently, is spelling out user stories"
- Shapes: part 1

### D37. Recommendations from what the person sees
- Date: 2026-11-21
- Place: conversation, item 8607
- Words: "but then why would we change it for codes? have you looked at the UX flow of the name registration before making such recommendation?"
- Shapes: questions

### D38. Conversation outranks written plans
- Date: 2026-11-24
- Place: conversation, item 4982
- Words: "your problem is that your give your written plan priority to our conversation, while it should be the opposite - any deviations should be fixed, any contradictions and gaps - discussed"
- Shapes: all parts

## Tests

### D39 (Q17). App tests: views against data, and one minimal end-to-end case
- Date: 2026-11-16
- Place: conversation, item 815
- Words: "That's why they need to be minimal and orthogonal, and we can't just adopt that PR wholesale - there are too many tests that can be better as unit tests. Even these tests - can we maybe test views against data, and have a minimal e2e test?"
- Shapes: part 3

## Answers that follow from the existing pictures

Each answer below follows from a picture of the existing UX design, named as its source: #7951's screens (`plans/2026-10-30-names-in-app-purchases/`, lanes 1 to 6, `names-life.svg`), the committed sketches (`plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, `plans/sketches/2026-09-23-badge-in-app-checkout.excalidraw`, `plans/2026-10-13-badge-subscription-renewals-sketches/`), or an earlier decision. Where a picture conflicts with a decision, the question stays.
- Date: 2026-11-27
- Place: SimpleX channel, items 16434 and 16447
- Words: "some of the things in the plan follow from UX design - is it not committed/" (item 16434); "but some of the answers follow from pictures - did you process them? or you try to avoid it?" (item 16447)

### D42 (Q5, Q26). One release, with buying, signing with the account keys, and restore from the 24 words
- Sources: `owner-decisions.md`, Process: "A staged release, a version-dependent UI, or states that members or views see differently are bad UX"; #7951 lane 6, screens 6·1 to 6·3; #7951 `names-life.svg`: "restore it on a new phone from the 24 words".
- Shapes: R17, flows 1 and 6

### D43 (Q4). Each payment is linked to its name, as on the names page, and the Privacy Policy states it
- Sources: #7951 section 11, Q6: "Register at once, fine for v1; `PRIVACY.md` states it"; #7951 section 2, non-goals: "Blind-signed codes".
- Shapes: D63, part 3

### D44 (Q23, Q12). A name bought on simplex.domains is brought in by its name code: moved into the app, or shown on the profile
- Sources: #7951 lane 3: 3·0, the site's "Open in SimpleX", the code filled in; 3·1 "Bring in a name", with "Move into this app" and "Show on your profile"; 3·2 Moving; 3·3 moved. The badge sketch, screen A5: the code leaves the page by link.
- Shapes: R19, flow 4

### D45 (Q25, Q29). For a restore, the app asks what each account holds, until 20 accounts in a row hold none, and adds the names to the active profile
- Sources: #7951 lane 6: 6·2 "Names found are added to Alice, the profile you are using now"; 6·2a "Looking for your names"; 6·3 "Stopped after 20 unused keys in a row"; #7951 section 6.10, the scan over the resolver's owned-names lookup. The badge sketch `restore.svg`: Restore moves a purchase to the active profile.
- Shapes: R17, flow 6

### D46 (Q27, Q28). SimpleX names under Advanced settings, per profile, with the device's recovery key; the 24 words shown later after the device's check
- Sources: #7951 lane 5: 5·1 Settings, Advanced settings, SimpleX names; 5·2 the profile's names, Restore purchases, the recovery key row; 5·3 "Your recovery key", Reveal; #7951 section 6.6, 1·4a, "The check".
- Shapes: R16, R18, flow 5

### D47 (Q6, Q8). Search answers are read from the name lookup; when a name is taken first, the payment is kept for another name
- Sources: the lookup sketch, 2c "Available" from the registry; #7951 1·5's re-check, "This name is no longer available"; #7951 lane 1b: 1·7 Taken, "What you paid is kept for another name"; 1·10 "Register with your payment"; #7951 `names-life.svg`: "if someone registers it first, the payment stays for another name".
- Shapes: R6, R7, flow 3

### D48 (Q10). A name may be registered to open nothing
- Sources: #7951 lane 1a, "1·5 · Opens nothing": the ✕ on Opens, "Nothing", "Undo", "Nobody will find you by it."; #7951 lane 5, 5·2: "Opens nothing".
- Shapes: R20

### D49 (Q13). After Register, Renew or Re-register in a name alert, the app shows its own flow
- Sources: the lookup sketch's footnote: "Register, Renew and Re-register open simplex.domains until the wallet (simplex-chat#7475) lands"; 4c "Renew opens the how-to until the wallet lands, then renews here"; #7951 1·1: the alerts' Register and Re-register open 1·2 with the name searched.
- Shapes: R8

### D50 (Q15). Unfinished name purchases are included in Restore purchases, as for badges
- Sources: #7951 lane 5, 5·2 "Restore purchases"; #7951 section 6.5, "a payment without a name → the search, the payment kept; a known name → its order"; the badge sketch `restore.svg`.
- Shapes: R4, R13

### D51 (Q24, in part). The 24 words are made and shown at the first Register on the device, before Check your order; the step is skipped for later orders
- Sources: #7951 lane 1a: 1·4 "Set up your recovery key", "Create a new key", "I already have a key"; 1·4a "Your key is created"; #7684's RFC, "Profiles and accounts": an account is created on first use.
- Shapes: R15, flow 1

### D52 (Q3, in part). One store product, $100 at the store's nearest price point, for 2 years
- Sources: D5; #7951 section 6.2: "SimpleX name", "Registers one name for 2 years", "$100, at the nearest price point".
- Shapes: R3

## The purchase flow, decided on the mockups

### D56. The screens are #7951's
- Decision: part 1's screens are #7951's images, with their numbers and words, changed only by the decisions below.
- Date: 2026-11-27 and 2026-11-28
- Place: SimpleX channel, items 17553, 17556 and 17647
- Words: "I am looking at your questions, but they look as if you completely ignored the existing plan and design." (item 17553); "\"3.product surface\" seems correct, it's consistent with designs and badges" (item 17556); "1-4 this design is your invention - we have an image for this screen, find it and replace." (item 17647)
- Shapes: part 1, section 4

### D66 (Q24). Saving the 24 words is optional, with a reminder
- Decision: as #7951 D12 and its 1·4a: Show and Copy on the sheet, Continue goes on, a reminder until Copy or Show.
- Date: 2026-11-28
- Place: SimpleX channel, items 17553 and 17647, on the mockup that draws it
- Words: "but in general yes, it is correct." (item 17647)
- Shapes: R15

### D67 (Q9). A hidden profile can't register a name
- Decision: as #7951 1·2: "A hidden profile can't register a name. Unhide it, or switch to another profile."
- Date: 2026-11-27
- Place: SimpleX channel, item 17553
- Words: "I am looking at your questions, but they look as if you completely ignored the existing plan and design."
- Shapes: R18

### D57. A code is entered on the search screen only
- Decision: I have a code on 1·2 is the only place to enter a code; 1·5, 4·1 and 4·2 offer no code.
- Date: 2026-11-28
- Place: SimpleX channel, items 17647 and 17765
- Words: "1-5 - we should not accept code here, it's enough to offer on screen 1. we must not get to pay $100 and they accept code in its place." (item 17647); "yes, and 4.1 too. I'd keep code only on screen 1-2." (item 17765)
- Shapes: R2

### D58. Pay in browser: under the store's Pay in the US, the main button where the app can't pay
- Decision: store builds in the US offer Pay in browser on 1·5, under Pay, for card, Bitcoin and Monero; builds that can't pay in the app pay on 4·2 in the browser; 1·2 is the same in every build.
- Date: 2026-11-28
- Place: SimpleX channel, items 17658, 17765 and 17782
- Words: "or maybe ok, let's keep \"Buy in browser\" as alternative, as they can pay BTC/XMR" (item 17658); "But if your proposal is that we sign transactions in the app, then we cannot have \"buy in browser\" in 1.2, and it will only be on 1.5, right? Which is better for names." (item 17765); "But these methods won't be offered in store-apps though, they can only send to the browser" (item 17782)
- Shapes: R8

### D59. The registration stages
- Decision: Ordering → Ordered, Waiting, Registering → Registered; a stage's label changes when its circle turns green.
- Date: 2026-11-28
- Place: SimpleX channel, item 17647
- Words: "Registration steps are unclear. \"Registering\" becomes \"Registered\" when it's circle becomes green. Does the design you see has these steps?"
- Shapes: R22

### D53 (Q11). The app validates a name over SMP, with no explorer link
- Date: 2026-11-27
- Place: SimpleX channel, items 17591 and 17597
- Words: "no it does not yet, it's a proposal and pr is not merged. We cannot do that. And I answered in artefact - app must resolve via SMP to validate" (item 17591); "no" (item 17597, to the proposal that the app shows the page's explorer link)
- Shapes: R23

### D60. The two-way check before a name shows as registered
- Decision: the app resolves the name and checks both directions: the name points to the address or channel, and the address or channel points to the name; 1·8 confirms the address.
- Date: 2026-11-28
- Place: SimpleX channel, item 17738
- Words: "it actually was a nice touch to reconfirm it. And the app should resolve it to see that it correctly updated both ways - name points to address, address points to name - so that anybody can now resolve it in the app. This validation via SMP resolver removes the need to send people to etherscan."
- Shapes: R23

### D61. The name address on screen with Developer tools only
- Date: 2026-11-28
- Place: SimpleX channel, item 17760
- Words: "yes, it's not needed, we can keep it with dev tools enabled though"
- Shapes: R11

### D54 (Q7). A store refund is followed by hand
- Decision: SimpleX tracks a disputed store payment and ends the name's registration; nothing ends it automatically.
- Date: 2026-11-27
- Place: SimpleX channel, item 17582
- Words: "no, we can track the dispute and end, but it can't be automatic"
- Shapes: R25

### D55. App users and page users differ
- Decision: the page opened from the app is adjusted to app users.
- Date: 2026-11-27
- Place: SimpleX channel, item 17600
- Words: "what you fail to understand is that page users and app users are different, and even when page is opened from the app it will have to adjust and change"
- Shapes: R26

### D62 (Q2). What the app pins, and what it reads
- Decision: the contracts and the chain are pinned in the app build; the terms and the prices come from the name lookup's answer (`nameTerms`, `pricing`); the term, a code's years included, and the dispute days come from the store service; the contract bounds the dispute days.
- Date: 2026-11-28
- Place: SimpleX channel, items 17765, 17782 and 17797
- Words: "contracts - yes, but terms can change - how can app pin them? and app has no chain access of its own, and we cannot do coordinated releases as we can do with the page." (item 17765); "what about investor codes? they can have longer terms"; "contract already restricts it, no need to duplicate in the app." (item 17782); "ah, you mean name record includes it already? then we can use it" (item 17797)
- Shapes: R9

### D63 (Q3). The browser pays the app's own order
- Decision: the app signs the order and one registration for each dispute period (card; Bitcoin and Monero) and puts them in the fragment of the link it opens; the page requests the invoice with the existing `POST /api/invoice` for the method picked; the app sends nothing before the page does; every request of the app goes to the store service over SMP, as badge requests go.
- Date: 2026-11-28
- Place: SimpleX channel, items 17815, 17819, 17821 and 17826
- Words: "unless the app signs all 3 payment methods and the page uses the one that is appropriate." (item 17815); "so invoice will be requested by the page, not by the app?" (item 17819); "why does the app needs to send anything to the service at this stage?" (item 17821); "even better" (item 17826, to two registrations in place of three orders); "app shouldn't be using POST, it should be using service RPC for everything. At least that we shouldn't simiplify. Look at badges." (item 17826)
- Shapes: R21, R24

### D64. In-app invoices later
- Decision: invoices requested by the app over SMP, for the privacy of the requests, come later, for badges too.
- Date: 2026-11-28
- Place: SimpleX channel, item 17826
- Words: "The argument for doing it all in-app is privacy of requests, but we can add it later, and for badges too. Web payment is useful anyway, and we have it."
- Shapes: R21

### D65. The page keeps nothing of an app order
- Decision: the page receives no key and lists the app's order nowhere in Your names.
- Date: 2026-11-28
- Place: SimpleX channel, item 17826
- Words: "You mean it'll show in browser's your names? I think it shouldn't, and browser shouldn't have private key in this case"
- Shapes: R21
