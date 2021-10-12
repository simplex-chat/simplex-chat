import 'package:simplexmq/src/buffer.dart';
import 'package:simplexmq/src/crypto.dart';
import 'package:test/test.dart';

void main() {
  group('AES-GCM encryption with padding', () {
    test('encrypt and decrypt', () {
      final key = AESKey.random();
      final iv = pseudoRandomBytes(16);
      final data = encodeAscii('hello');
      final cipherText = encryptAES(key, iv, 32, data);
      expect(cipherText.length, 32 + 16);
      final decrypted = decryptAES(key, iv, cipherText);
      expect(decodeAscii(decrypted),
          'hello' + List.filled(32 - 'hello'.length, '#').join());
    });
  });
}
