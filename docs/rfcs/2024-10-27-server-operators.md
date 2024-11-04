# Server operators

## Problem

All preconfigured servers operated by a single company create a risk that user connections can be analysed by aggregating transport information from these servers.

The solution is to have more than one operator servers pre-configured in the app.

For operators to be protected from any violations of rights of other users or third parties by the users who use servers of these operators, the users have to explicitely accept conditions of use with the operator, in the same way they accept conditions of use with SimpleX Chat Ltd by downloading the app.

## Solution

Allow to assign operators to servers, both with preconfigured operators and servers, and with user-defined operators. Agent added support for server roles, chat app could:
- allow assigning server roles only on the operator level.
- only on server level.
- on both, with server roles overriding operator roles (that would require a different type for server for chat app).

For simplicity of both UX and logic it is probably better to allow assigning roles only on operators' level, and servers without set operators can be used for both roles.

For agreements, it is sufficient to record the signatures of these agreements on users' devices, together with the copy of signed agreement (or its hash and version) in a separate table. The terms themselves could be:
- included in the app - either in code or in migration.
- referenced with a stable link to a particular commit.

The first solution seems better, as it avoids any third party dependency, and the agreement size is relatively small (~31kb), to reduce size we can store it compressed.
