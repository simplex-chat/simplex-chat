import 'dart:typed_data';
import 'package:asn1lib/asn1lib.dart';
import 'package:pointycastle/asymmetric/api.dart'
    show RSAPublicKey, RSAPrivateKey;

const _rsaOid = '1.2.840.113549.1.1.1';
final _asnRsaOid = ASN1ObjectIdentifier.fromComponentString(_rsaOid);
final _asn1Null = ASN1Null();

ASN1Sequence _asn1Sequence(List<ASN1Object> elements) {
  final seq = ASN1Sequence()..elements = elements;
  return seq;
}

void _assertRsaAlgorithm(ASN1Sequence seq) {
  if (seq.elements.isEmpty ||
      seq.elements[0] is! ASN1ObjectIdentifier ||
      (seq.elements[0] as ASN1ObjectIdentifier).identifier != _rsaOid) {
    throw Exception('Invalid key algorithm identifier');
  }
}

/// Decodes binary PKCS1 to [RSAPublicKey]
RSAPublicKey decodeRsaPubKeyPKCS1(Uint8List bytes) {
  final els = ASN1Sequence.fromBytes(bytes).elements;
  if (els.length != 2 || els[0] is! ASN1Integer || els[1] is! ASN1Integer) {
    throw Exception('Invalid PKCS1 encoding');
  }
  return RSAPublicKey(
    (els[0] as ASN1Integer).valueAsBigInteger!,
    (els[1] as ASN1Integer).valueAsBigInteger!,
  );
}

/// Decodes binary SPKI to [RSAPublicKey]
RSAPublicKey decodeRsaPubKey(Uint8List bytes) {
  final els = ASN1Sequence.fromBytes(bytes).elements;
  if (els.length != 2 || els[1] is! ASN1BitString || els[0] is! ASN1Sequence) {
    throw Exception('Invalid SPKI structure');
  }
  _assertRsaAlgorithm(els[0] as ASN1Sequence);
  return decodeRsaPubKeyPKCS1(els[1].valueBytes().sublist(1));
}

/// Encodes [key] as binary PKCS1
Uint8List encodeRsaPubKeyPKCS1(RSAPublicKey key) =>
    _asn1Sequence([ASN1Integer(key.modulus!), ASN1Integer(key.publicExponent!)])
        .encodedBytes;

/// Encodes [key] as binary SPKI
Uint8List encodeRsaPubKey(RSAPublicKey key) => _asn1Sequence([
      _asn1Sequence([_asnRsaOid, _asn1Null]),
      ASN1BitString(encodeRsaPubKeyPKCS1(key))
    ]).encodedBytes;

/// Decodes binary PKCS1 to [RSAPrivateKey]
RSAPrivateKey decodeRsaPrivKeyPKCS1(Uint8List bytes) {
  final els = ASN1Sequence.fromBytes(bytes).elements;
  if (els.length != 9 || els.any((el) => el is! ASN1Integer)) {
    throw Exception('Invalid PKCS1 encoding');
  }
  return RSAPrivateKey(
      (els[1] as ASN1Integer).valueAsBigInteger!,
      (els[3] as ASN1Integer).valueAsBigInteger!,
      (els[4] as ASN1Integer).valueAsBigInteger!,
      (els[5] as ASN1Integer).valueAsBigInteger!);
}

/// Decodes binary PKCS8 to [RSAPrivateKey]
RSAPrivateKey decodeRsaPrivKey(Uint8List bytes) {
  final els = ASN1Sequence.fromBytes(bytes).elements;
  if (els.length != 3 ||
      els[1] is! ASN1Sequence ||
      els[2] is! ASN1OctetString) {
    throw Exception('Invalid PKCS8 structure');
  }
  _assertRsaAlgorithm(els[1] as ASN1Sequence);
  return decodeRsaPrivKeyPKCS1(els[2].valueBytes());
}

final _asnZero = ASN1Integer(BigInt.from(0));

/// Encodes [key] as PKCS1 binary
Uint8List encodeRsaPrivKeyPKCS1(RSAPrivateKey key) {
  final d = key.privateExponent!;
  final p = key.p!;
  final q = key.q!;
  final dModP = d % (p - BigInt.from(1));
  final dModQ = d % (q - BigInt.from(1));
  final coefficient = q.modInverse(p);
  return _asn1Sequence([
    _asnZero,
    ASN1Integer(key.modulus!),
    ASN1Integer(key.publicExponent!),
    ASN1Integer(d),
    ASN1Integer(p),
    ASN1Integer(q),
    ASN1Integer(dModP),
    ASN1Integer(dModQ),
    ASN1Integer(coefficient),
  ]).encodedBytes;
}

/// Encodes [key] as PKCS8 binary
Uint8List encodeRsaPrivKey(RSAPrivateKey key) => _asn1Sequence([
      _asnZero,
      _asn1Sequence([_asnRsaOid, _asn1Null]),
      ASN1OctetString(encodeRsaPrivKeyPKCS1(key))
    ]).encodedBytes;
