import 'dart:typed_data';
import 'package:asn1lib/asn1lib.dart';
import 'package:pointycastle/asymmetric/api.dart';
import 'package:ssh_key/ssh_key_bin.dart';

const _rsaAlgorithmOid = '1.2.840.113549.1.1.1';
final asn1Null = ASN1Null();

RSAPublicKey? decodePubKey(Uint8List binKey) {
  // final source = PubTextSource('', 0, 0, PubKeyEncoding.x509spki);
  final spki = SubjectPublicKeyInfo.decode(binKey, source: null);
  if (spki.algorithmOid != _rsaAlgorithmOid ||
      spki.algorithmParameters.length != 1 ||
      spki.algorithmParameters.first is! ASN1Null) return null;
  final pkcs1 = Pkcs1RsaPublicKey.decode(spki.data, null);
  return RSAPublicKey(pkcs1.modulus, pkcs1.exponent);
}

Uint8List encodePubKey(RSAPublicKey key) {
  final seq = ASN1Sequence()
    ..add(ASN1Integer(key.modulus!))
    ..add(ASN1Integer(key.publicExponent!));
  final spki =
      SubjectPublicKeyInfo(_rsaAlgorithmOid, [asn1Null], seq.encodedBytes);
  return spki.encode();
}
