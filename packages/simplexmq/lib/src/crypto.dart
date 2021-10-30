import 'dart:math' show Random;
import 'dart:typed_data';
import 'package:pointycastle/api.dart';
import 'package:pointycastle/asymmetric/api.dart';
import 'package:pointycastle/asymmetric/oaep.dart';
import 'package:pointycastle/asymmetric/rsa.dart';
import 'package:pointycastle/block/aes_fast.dart';
import 'package:pointycastle/block/modes/gcm.dart';
import 'package:pointycastle/digests/sha256.dart';
import 'package:pointycastle/key_generators/api.dart';
import 'package:pointycastle/key_generators/rsa_key_generator.dart';
import 'package:pointycastle/random/fortuna_random.dart';
import 'package:pointycastle/signers/pss_signer.dart';
import 'buffer.dart';

class AESKey {
  final Uint8List _key;
  AESKey._make(this._key);

  static AESKey random([bool secure = false]) =>
      AESKey._make((secure ? secureRandomBytes : pseudoRandomBytes)(32));

  static AESKey decode(Uint8List rawKey) => AESKey._make(rawKey);

  Uint8List get bytes => _key;
}

Uint8List randomIV() => pseudoRandomBytes(16);

Uint8List secureRandomBytes(int len) => _randomBytes(len, Random.secure());

final sessionSeed = Random.secure();

Uint8List pseudoRandomBytes(int len) => _randomBytes(len, sessionSeed);

Uint8List _randomBytes(int len, Random seedSource) {
  final bytes = Uint8List(len);
  for (int i = 0; i < len; i++) {
    bytes[i] = seedSource.nextInt(256);
  }
  return bytes;
}

final paddingByte = '#'.codeUnitAt(0);

const int _macBytes = 16;
const int _macBits = _macBytes * 8;

Uint8List encryptAES(AESKey key, Uint8List iv, int blockSize, Uint8List data) {
  final padTo = blockSize - _macBytes;
  if (data.length >= padTo) throw ArgumentError('large message');
  final padded = Uint8List(padTo);
  padded.setAll(0, data);
  padded.fillRange(data.length, padTo, paddingByte);
  return _makeGCMCipher(key, iv, true).process(padded);
}

Uint8List decryptAES(AESKey key, Uint8List iv, Uint8List encryptedAndTag) =>
    _makeGCMCipher(key, iv, false).process(encryptedAndTag);

GCMBlockCipher _makeGCMCipher(AESKey key, Uint8List iv, bool encrypt) =>
    GCMBlockCipher(AESFastEngine())
      ..init(
          encrypt, AEADParameters(KeyParameter(key._key), _macBits, iv, empty));

FortunaRandom _secureFortunaRandom() =>
    FortunaRandom()..seed(KeyParameter(secureRandomBytes(32)));

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

Uint8List encryptOAEP(RSAPublicKey key, Uint8List data) =>
    _oaep(true, PublicKeyParameter<RSAPublicKey>(key)).process(data);

Uint8List decryptOAEP(RSAPrivateKey key, Uint8List data) =>
    _oaep(false, PrivateKeyParameter<RSAPrivateKey>(key)).process(data);

OAEPEncoding _oaep(bool encrypt, AsymmetricKeyParameter keyParam) =>
    OAEPEncoding.withSHA256(RSAEngine())..init(encrypt, keyParam);

Uint8List signPSS(RSAPrivateKey privateKey, Uint8List data) =>
    _pss(true, PrivateKeyParameter<RSAPrivateKey>(privateKey))
        .generateSignature(data)
        .bytes;

bool verifyPSS(RSAPublicKey publicKey, Uint8List data, Uint8List sig) {
  try {
    return _pss(false, PublicKeyParameter<RSAPublicKey>(publicKey))
        .verifySignature(data, PSSSignature(sig));
  } on ArgumentError {
    return false;
  }
}

PSSSigner _pss(bool sign, AsymmetricKeyParameter keyParam) => PSSSigner(
    RSAEngine(), SHA256Digest(), SHA256Digest())
  ..init(sign,
      ParametersWithSaltConfiguration(keyParam, _secureFortunaRandom(), 32));
