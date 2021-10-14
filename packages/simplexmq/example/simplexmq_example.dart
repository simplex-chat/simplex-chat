import 'dart:typed_data';
import 'package:asn1lib/asn1lib.dart';
import 'package:pointycastle/asymmetric/api.dart';
import 'package:simplexmq/src/buffer.dart';
import 'package:ssh_key/ssh_key.dart';
import 'package:ssh_key/ssh_key_bin.dart';
import 'package:ssh_key/ssh_key_txt.dart';

void main() {
  // final k = publicKeyDecode('''
  // -----BEGIN PUBLIC KEY-----
  // MIIBoDANBgkqhkiG9w0BAQEFAAOCAY0AMIIBiAKCAQEAsigXn8PsL+tyXVOXNhcIFuHmH/aU42ZZoaIEyt8/4MZrype9HD90o9s0bTObdiW4AAKoDG0hvwpjwQeLSTyMdIKqfntzw+yI/qrq+nsV35m+Xcy+HD91Q5eWcf3O6886N9cBTH2ThQ2an3+7z6T4yr58ybrUMs0sM7QKOKj6EOdEkUyRFJhqdhhuCyZPh+HPMEBj2qSJqkkBWEWRoEQULfnyDaEdQVZPCngQc9ixbZlajWDPoKXlKlfAVEIXMIWHiNAqqxOOuHx0+sv9DuWHxy+3WFuGRIPhNEz1oIYs4lqpkZ8wb2S+Og+wwsJbHgLxTsQErrnLuTEtIAa06u1q0wKBgHgT22txXS4nHmTQUEsUEa6In1AzNAsyTJwETBl/jYXHOtDUR4gH5sDVnzSpnawVG7ZerW7tBCXjMRMCzmv539sLbYvEslRUNXOaSFCGnImC1ImNIn5bjssGcMFSNEe1gNPnTTwWCuKoCQ3MqFduxJd+CT2fKAMTSMv0MsGtEf0Z
  // -----END PUBLIC KEY-----
  // ''');
  final keyStr =
      'MIIBoDANBgkqhkiG9w0BAQEFAAOCAY0AMIIBiAKCAQEAsigXn8PsL+tyXVOXNhcIFuHmH/aU42ZZoaIEyt8/4MZrype9HD90o9s0bTObdiW4AAKoDG0hvwpjwQeLSTyMdIKqfntzw+yI/qrq+nsV35m+Xcy+HD91Q5eWcf3O6886N9cBTH2ThQ2an3+7z6T4yr58ybrUMs0sM7QKOKj6EOdEkUyRFJhqdhhuCyZPh+HPMEBj2qSJqkkBWEWRoEQULfnyDaEdQVZPCngQc9ixbZlajWDPoKXlKlfAVEIXMIWHiNAqqxOOuHx0+sv9DuWHxy+3WFuGRIPhNEz1oIYs4lqpkZ8wb2S+Og+wwsJbHgLxTsQErrnLuTEtIAa06u1q0wKBgHgT22txXS4nHmTQUEsUEa6In1AzNAsyTJwETBl/jYXHOtDUR4gH5sDVnzSpnawVG7ZerW7tBCXjMRMCzmv539sLbYvEslRUNXOaSFCGnImC1ImNIn5bjssGcMFSNEe1gNPnTTwWCuKoCQ3MqFduxJd+CT2fKAMTSMv0MsGtEf0Z';
  final k = decodePubKey(encodeAscii(keyStr));
  // final k = privateKeyDecode('MIIFIQIBAAKCAQEAsigXn8PsL+tyXVOXNhcIFuHmH/aU42ZZoaIEyt8/4MZrype9HD90o9s0bTObdiW4AAKoDG0hvwpjwQeLSTyMdIKqfntzw+yI/qrq+nsV35m+Xcy+HD91Q5eWcf3O6886N9cBTH2ThQ2an3+7z6T4yr58ybrUMs0sM7QKOKj6EOdEkUyRFJhqdhhuCyZPh+HPMEBj2qSJqkkBWEWRoEQULfnyDaEdQVZPCngQc9ixbZlajWDPoKXlKlfAVEIXMIWHiNAqqxOOuHx0+sv9DuWHxy+3WFuGRIPhNEz1oIYs4lqpkZ8wb2S+Og+wwsJbHgLxTsQErrnLuTEtIAa06u1q0wKBgHgT22txXS4nHmTQUEsUEa6In1AzNAsyTJwETBl/jYXHOtDUR4gH5sDVnzSpnawVG7ZerW7tBCXjMRMCzmv539sLbYvEslRUNXOaSFCGnImC1ImNIn5bjssGcMFSNEe1gNPnTTwWCuKoCQ3MqFduxJd+CT2fKAMTSMv0MsGtEf0ZAoIBAG+ACItCBcQNr5eEQbmo3C+pMS4q6OnOvNjdpUGkkIkDcfv0jElmZc6yP3/DX3jAutoVG2IUlEKKH02SqBqIyryiUk8GjQg5ZpZ5ISV2Zbp5yRRwfvBYv+9VwoCt4O74B+dfHrI9V1eRPK68A+/LTdLSj5NbQArmydFsOugab+5D0EGRVuo/23J3k9opwJDd9w+8kEkBSTIzO4oLx2PwGxRqqLXGysDrRR42PG+btVWGZfgZ2GORsOdb0/8g7xpYdZ2fC4bbzmVVNNnJr682SwUTmDa7C2m2JwZ7zE0DsRo59asp9wy7uG/OXCLYrBHI9srVAiq3IlsFDHZ9FKS2wQkCgYEA6MaoBKVeC+vf5EshmJlopx9o/SBBZ+w1Qu+pPfyScgVUEi17PWU6Igtpe4dU+qX8pTNrh5BQ+ur7KJpxbeyPdawSDV1X3nEVcNj4kBfEnW/fEtWeSTTAX6AIsJPzAdO0W7LfD7B+tLG1FTTopGoqyYrIpwixETdEC5oFg4FRf0UCgYEAw+5mOiUMS70rZ+hlmTtT6oOC8UZEliDxJFCOVyC5vuCIDAPP+jTHMFfGgHfkSryMRJpVq5r6TTJFq6ViOhwTqj9QlP0NxlCyhc9l4+tbpz84+jJudF1RlagBqaH/wrWbRHarIqS6I77t5JEG4dk4cEXRZ2Non6BkwDwSLZpadzcCgYAsMRBJKbfD9e68RPrTRR6JPLsTrolXwJR247EXsvWa4b1W6i+Ppv29/Y5tokSeY5+7Fq7Spot5nEn2CpliDoz2LHeMQX23nQr3iujou8Jwte/HA6tx1MxWiKd3OrFkpuQujxxd8O02nLo8NSc2Hkbzq8Yq2hhniTP1npPsXKZtPQKBgAZlV1dDVc9wvEdnut5RMRERsDJXb5U+sB3dZTtTfjFBqd8GJT1Ekgzf5n+GwTgnISScRWwlOzY3sNy1XwYKlteL+IXLAYzKhZRdAUocUJva+pfUrhHJWsXYcy6liSVkYtN3Utm4KvDid36EujqLY+wEzytyzu84y3ijB6oEEDTXAoGBALxl9NBo9m/au0n/62f2E7G1ReFW2SJLjWPaBgt1yGaVm0gFi4JmW3WWU6FZ/QPsxz9fzOTGGFPDhRPjYFR8o7Dqn+XnxHw8VCw/d2DcGJTXG3biT96JOQGNnnFg+/r/PKZGt+ltCT9Q4xFfMYocWRAVtqi0RuKNs3qx8LxnVaYM');
  print(k);
  final encKey = decodeAscii(encodePubKey(k!));
  print(encKey);
  print(encKey == keyStr);
}

const _rsaAlgorithmOid = '1.2.840.113549.1.1.1';

RSAPublicKey? decodePubKey(Uint8List s) {
  final bytes = decode64(s);
  if (bytes == null) return null;
  // final source = PubTextSource('', 0, 0, PubKeyEncoding.x509spki);
  final spki = SubjectPublicKeyInfo.decode(bytes, source: null);
  if (spki.algorithmOid != _rsaAlgorithmOid ||
      spki.algorithmParameters.length != 1 ||
      spki.algorithmParameters.first is! ASN1Null) return null;
  final pkcs1 = Pkcs1RsaPublicKey.decode(spki.data, null);
  return RSAPublicKey(pkcs1.modulus, pkcs1.exponent);
}

final asn1Null = ASN1Null();

Uint8List encodePubKey(RSAPublicKey key) {
  final seq = ASN1Sequence()
    ..add(ASN1Integer(key.modulus!))
    ..add(ASN1Integer(key.publicExponent!));
  final spki =
      SubjectPublicKeyInfo(_rsaAlgorithmOid, [asn1Null], seq.encodedBytes);
  return encode64(spki.encode());
}
