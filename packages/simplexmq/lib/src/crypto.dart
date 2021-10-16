import 'dart:math' show Random;
import 'dart:typed_data';
import 'package:pointycastle/api.dart';
import 'package:pointycastle/asymmetric/api.dart';
import 'package:pointycastle/asymmetric/oaep.dart';
import 'package:pointycastle/asymmetric/rsa.dart';
import 'package:pointycastle/block/aes_fast.dart';
import 'package:pointycastle/block/modes/gcm.dart';
import 'package:pointycastle/key_generators/api.dart';
import 'package:pointycastle/key_generators/rsa_key_generator.dart';
import 'package:pointycastle/random/fortuna_random.dart';

class AESKey {
  final Uint8List _key;
  AESKey._make(this._key);

  static AESKey random([bool secure = false]) =>
      AESKey._make((secure ? secureRandomBytes : pseudoRandomBytes)(32));

  static AESKey decode(Uint8List rawKey) => AESKey._make(rawKey);

  Uint8List get bytes => _key;
}

Uint8List randomIV() {
  return pseudoRandomBytes(16);
}

Uint8List secureRandomBytes(int len) {
  return _randomBytes(len, Random.secure());
}

final sessionSeed = Random.secure();

Uint8List pseudoRandomBytes(int len) {
  return _randomBytes(len, sessionSeed);
}

// len should be divisible by 4
Uint8List _randomBytes(int len, Random seedSource) {
  final bytes = Uint8List(len);
  for (int i = 0; i < len; i++) {
    bytes[i] = seedSource.nextInt(256);
  }
  return bytes;
}

final empty = Uint8List(0);
final paddingByte = '#'.codeUnitAt(0);

Uint8List encryptAES(AESKey key, Uint8List iv, int padTo, Uint8List data) {
  if (data.length >= padTo) throw ArgumentError('large message');
  final padded = Uint8List(padTo);
  padded.setAll(0, data);
  padded.fillRange(data.length, padTo, paddingByte);
  return _makeGCMCipher(key, iv, true).process(padded);
}

Uint8List decryptAES(AESKey key, Uint8List iv, Uint8List encryptedAndTag) {
  return _makeGCMCipher(key, iv, false).process(encryptedAndTag);
}

GCMBlockCipher _makeGCMCipher(AESKey key, Uint8List iv, bool encrypt) {
  return GCMBlockCipher(AESFastEngine())
    ..init(encrypt, AEADParameters(KeyParameter(key._key), 128, iv, empty));
}

FortunaRandom _secureFortunaRandom() {
  return FortunaRandom()..seed(KeyParameter(secureRandomBytes(32)));
}

AsymmetricKeyPair<RSAPublicKey, RSAPrivateKey> generateRSAkeyPair(
    [int bitLength = 2048]) {
  final keyGen = RSAKeyGenerator()
    ..init(ParametersWithRandom(
        RSAKeyGeneratorParameters(BigInt.parse('65537'), bitLength, 64),
        _secureFortunaRandom()));
  final pair = keyGen.generateKeyPair();
  return AsymmetricKeyPair<RSAPublicKey, RSAPrivateKey>(
      pair.publicKey as RSAPublicKey, pair.privateKey as RSAPrivateKey);
}

Uint8List encryptOAEP(RSAPublicKey key, Uint8List data) {
  final oaep = OAEPEncoding(RSAEngine())
    ..init(true, PublicKeyParameter<RSAPublicKey>(key));
  return oaep.process(data);
}

Uint8List decryptOAEP(RSAPrivateKey key, Uint8List data) {
  final oaep = OAEPEncoding(RSAEngine())
    ..init(false, PrivateKeyParameter<RSAPrivateKey>(key));
  return oaep.process(data);
}
