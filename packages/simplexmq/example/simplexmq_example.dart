import 'dart:typed_data';

import 'package:simplexmq/src/buffer.dart';
import 'package:simplexmq/src/crypto.dart';
import 'package:simplexmq/src/rsa_keys.dart';
// import 'package:simplexmq/src/transport.dart';
// import 'dart:io';
// import 'package:rsa_pkcs/rsa_pkcs.dart' show RSAPKCSParser;
// import 'package:pointycastle/asymmetric/api.dart';
// import 'package:ssh_key/ssh_key.dart';

void main() async {
  // final socket = await Socket.connect('localhost', 8080);
  // socket.add([65, 66, 67]);
  // await socket.flush();
  final keyStr =
      // 'MIIBoDANBgkqhkiG9w0BAQEFAAOCAY0AMIIBiAKCAQEAtfalLDtEveHNlKq782D9kIRYbICBckd+v83HeAs7MI/u39fBRmk8kHZ8k/OzN5o+aBWDoBAm7m+24fnolR2QwQG4WSSLPLr3uXQDm7hHWHCu5TqNryf70QlJAbdljnwWauLxWwJ9bfCtjnBPBxL1Aypd/stIZJA4qKpuOtIMBh9n8RzuyRFEKGTrDguTOVTFK2R3/DF10G1LmU2cKC/QTCt/fVJsjihfivTanrNri8ue0Hs3pK+sL7L5zlN7srnU2CZHZtuoXldx1LRIn3Y1Rj33ASpfM7gDkndWjqdK6t5FQdxe3R+j5txqg8EMTb9PF4PfhdvEi9f7238AovMeowKBgAtXNGoxBEvCfVQbM9RCYS90nPIEfgV3D7jK9HrrvCewYnU55fhTsNZFYQNj/iq3m9t6VTG/helpM6bK4rLvW5YAY+1dVltHErPODy3Tz+3Fg7aP7EZ1XrPjHfCd87zTr5/ima4GZIxutbDfNd1WecIIK9gVrstSjG/fIuQ3Ay6X';
      'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvV/IhWtgS1CENNl38Jo9mJqeJ63agN8zo2PAIHNxuorlFUpaXOkLtHcASn3PiXwjNgFuO2FtiP1QRIz7Cg8bACHtdsA20moUCnfExOgwhHsePCFxxcWTfPTeYqb83OJXKJdc3t0ihPD9Zo2o75BY6ahSiNY4NLQHEnuqCK+P/ItUDa7QqqzHcinlRPVgO7HbE/5YsGafd7McHVmd8dv0nild0y29SILXvGKOvHIkUBthb32VaCI+75b9A1eNvLdUlF0kSmWzQTLjSYwWVSvfP0zAetZ1vlksFaohYrSZUtZ0PlTkti1rdyCMdvLpsS2A6g3vjuobm0oUKiRgikNYywIDAQAB';
  // final pair = RSAPKCSParser().parsePEM('''
//   final key = publicKeyDecode('''
// -----BEGIN PUBLIC KEY-----
// MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAzknLODTvx9LrwnWrgoe5zSF7olPnp20O/0PLw/WtvV9paLzkeyhf07RqBzYUEyGccpOMEwY98SsJk/8GJnVPUclAHj2rqxAkwYTKHbsMjbZ18VuL0vC5JTBwnvqMUa+fj3oi7QxHMWLY9w1iYLauYM/aYWBc/Fg5LZoawG0Yf3nuZ3rGWic88tf5y8BIVgSEeRovF/NQKX8LsFgaxXdpTMfMwu7xVh0jaFUU98T6ke+GADszmuqeUXFs9IRZoRUieTSafM8XQ+Xr7g513TYTpXuj9GQ04nmE+BEZ5O2MBNjYFle91SsvCyPwYxv0aFbFZ05/vZKZIdbhgpFxh03p3wIDAQAB
// -----END PUBLIC KEY-----
// ''') as RSAPublicKey;
  // final key = RSAPublicKey((pair.public?.modulus) as BigInt,
  //     BigInt.from((pair.public?.publicExponent) as int));

  final key = decodeRsaPubKey(decode64(encodeAscii(keyStr))!);
  print(key);
  final Uint8List header = encodeAscii('hello');
  //  concatN([
  //   encodeInt32(4096),
  //   encodeInt16(0),
  //   SessionKey.create().serialize(),
  //   SessionKey.create().serialize()
  // ]);
  final enc = decodeAscii(encode64(encryptOAEP(key, header)));
  print(enc);
}
