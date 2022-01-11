To generate fixtures:

(keep these instructions and *openssl.cnf* consistent with certificate generation on server)

```sh
# CA certificate (identity/offline)
openssl genpkey -algorithm ED448 -out ca.key
openssl req -new -x509 -days 999999 -config openssl.cnf -extensions v3_ca -key ca.key -out ca.crt
# server certificate (online)
openssl genpkey -algorithm ED448 -out server.key
openssl req -new -config openssl.cnf -reqexts v3_req -key server.key -out server.csr
openssl x509 -req -days 999999 -extfile openssl.cnf -extensions v3_req -in server.csr -CA ca.crt -CAkey ca.key -CAcreateserial -out server.crt
# to pretty-print
openssl x509 -in ca.crt -text -noout
openssl req -in server.csr -text -noout
openssl x509 -in server.crt -text -noout
```

To compute fingerprint for tests:

```haskell
stack ghci
> import Data.X509.Validation (Fingerprint (..))
> Fingerprint fp <- loadFingerprint "tests/fixtures/ca.crt"
> strEncode fp
```
