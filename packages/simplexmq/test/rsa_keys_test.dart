import 'package:pointycastle/pointycastle.dart';
import 'package:simplexmq/src/buffer.dart';
import 'package:simplexmq/src/rsa_keys.dart';
import 'package:test/test.dart';

void main() {
  group('RSA keys encoding', () {
    test('encode/decode RSA public key', () {
      final keyStr =
          'MIIBoDANBgkqhkiG9w0BAQEFAAOCAY0AMIIBiAKCAQEAsigXn8PsL+tyXVOXNhcIFuHmH/aU42ZZoaIEyt8/4MZrype9HD90o9s0bTObdiW4AAKoDG0hvwpjwQeLSTyMdIKqfntzw+yI/qrq+nsV35m+Xcy+HD91Q5eWcf3O6886N9cBTH2ThQ2an3+7z6T4yr58ybrUMs0sM7QKOKj6EOdEkUyRFJhqdhhuCyZPh+HPMEBj2qSJqkkBWEWRoEQULfnyDaEdQVZPCngQc9ixbZlajWDPoKXlKlfAVEIXMIWHiNAqqxOOuHx0+sv9DuWHxy+3WFuGRIPhNEz1oIYs4lqpkZ8wb2S+Og+wwsJbHgLxTsQErrnLuTEtIAa06u1q0wKBgHgT22txXS4nHmTQUEsUEa6In1AzNAsyTJwETBl/jYXHOtDUR4gH5sDVnzSpnawVG7ZerW7tBCXjMRMCzmv539sLbYvEslRUNXOaSFCGnImC1ImNIn5bjssGcMFSNEe1gNPnTTwWCuKoCQ3MqFduxJd+CT2fKAMTSMv0MsGtEf0Z';
      final key = decodePubKey(decode64(encodeAscii(keyStr))!);
      expect(key is RSAPublicKey, true);
      final encKey = decodeAscii(encode64(encodePubKey(key!)));
      expect(encKey, keyStr);
    });
  });
}
