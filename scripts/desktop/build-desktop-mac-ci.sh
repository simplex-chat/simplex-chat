#!/bin/bash

set -e

trap "rm apps/multiplatform/local.properties || true; rm local.properties || true; rm /tmp/simplex.keychain || true" EXIT
echo "desktop.mac.signing.identity=Developer ID Application: SimpleX Chat Ltd (5NN7GUYB6T)" >> apps/multiplatform/local.properties
echo "desktop.mac.signing.keychain=/tmp/simplex.keychain" >> apps/multiplatform/local.properties
echo "desktop.mac.notarization.apple_id=$APPLE_SIMPLEX_NOTARIZATION_APPLE_ID" >> apps/multiplatform/local.properties
echo "desktop.mac.notarization.password=$APPLE_SIMPLEX_NOTARIZATION_PASSWORD" >> apps/multiplatform/local.properties
echo "desktop.mac.notarization.team_id=5NN7GUYB6T" >> apps/multiplatform/local.properties
echo "$APPLE_SIMPLEX_SIGNING_KEYCHAIN" | base64 --decode -o /tmp/simplex.keychain

scripts/desktop/build-lib-mac.sh
cd apps/multiplatform
./gradlew packageDmg
./gradlew notarizeDmg
