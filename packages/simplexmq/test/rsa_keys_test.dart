import 'package:simplexmq/src/buffer.dart';
import 'package:simplexmq/src/rsa_keys.dart';
import 'package:test/test.dart';

void main() {
  group('RSA keys encoding', () {
    test('SPKI encode/decode RSA public key', () {
      final keyStr =
          'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2fdE2lndTnPi7QHS2OqP1YE6ZH6sGdf7Boji6jPgQVJB289aQAPZRSJlg6s+xHC52sa2isFiuZN2uFENNWznuZsOWXkHMthbo9Qkp7ZjOhomZURtsIsaRny9GTcaFOrd19rqbsrCRLyb3xtwbQjv/2HEGNZyP9YsGsZijTJaV0yQNEp/5Gt3jHebJ8mqLdBr/aDQBf3oSsmUDDvocGU4kL14GOuVYCKNlEUrFe1X1poSXLH0uu485GVfHB72XjKP/flS2rL91fguqMil1nkelL1K4WOyx1Z87LyyXT2Vh4GRLVHG/a9LyPpw7ovQlO5RIr6suODkXwbAUHq/8j5IDwIDAQAB';
      final key = decodeRsaPubKey(decode64(encodeAscii(keyStr))!);
      final encKey = decodeAscii(encode64(encodeRsaPubKey(key)));
      expect(encKey, keyStr);
    });

    test('PKCS8 encode/decode RSA private key', () {
      final keyStr =
          'MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQDZ90TaWd1Oc+LtAdLY6o/VgTpkfqwZ1/sGiOLqM+BBUkHbz1pAA9lFImWDqz7EcLnaxraKwWK5k3a4UQ01bOe5mw5ZeQcy2Fuj1CSntmM6GiZlRG2wixpGfL0ZNxoU6t3X2upuysJEvJvfG3BtCO//YcQY1nI/1iwaxmKNMlpXTJA0Sn/ka3eMd5snyaot0Gv9oNAF/ehKyZQMO+hwZTiQvXgY65VgIo2URSsV7VfWmhJcsfS67jzkZV8cHvZeMo/9+VLasv3V+C6oyKXWeR6UvUrhY7LHVnzsvLJdPZWHgZEtUcb9r0vI+nDui9CU7lEivqy44ORfBsBQer/yPkgPAgMBAAECggEAezFPgB4EgB/tpUk/k4xXiTPF/iC+QskYvyPFJNv3JtRIFuWGO+Iw/esn9xhlnH+d+/IOIDSXCQ44ropY7dZEzlm97YIDOJCikuEHaqciRCedheT8Hikwy6Aa/NJw8lug0SyRDdeZn2H+s0X98BJ6Gxx1yhgCcOQq/2MbNnS8LNQ0yNNHu9Ds2K5Weiwhb9nrLLuMvrF/k1z0QNi5mCzDZK8iDMr87UZycmKKue13/xppI1pddJm4Ta13/OmZtYe2d5UgK9FrLStFkl7yqWnIcDueCOZvqo4nIfxPlPVolQ9B9RXL2tctkYRVy6FBkZIJkSk4O1Vz5BuPgBy9McoRMQKBgQD5xmVKAnvBQ08BUGXw5HVy4qR1Oj/EmxXKNypZsTdaKpiMfjNB8GbegO0na9Ry7sRo5g15vXpjCS6LDwGx4fZ/5u4N5CP3845DscrZjebs0tS7u5USPMoMzZ/KYfddRsGdm7y9HMp3Z6O3ZGGqZ15VFHGYjRjzK8BYE91rvv1WlwKBgQDfZe0c8/aa25KHW2zLRtSOR8ze+pz79hxNmpCeOoyzfaVRzwDh3KUl78PEWqMbJ7EdEdokCisFU0yEpuuc29JD6l9YuQmYH2VGdyg52iPPCXJOP1PBO0VQ/D/cAmZd/75cmoiyC7kELHfiAWBqO/7xpWkNiEpZZcI33DbzReYBSQKBgBKGuaqUppM+J9UEHpuQhnmf/+zGBkbR7frSvqxqbZ2dfTUmgyzH5Qlp7K043UgtF5pkPemiuToxSyd7VHfaN8ti2JNlMZnJkerJfC9IzDESrj7CehshMSdj9Q8w1wUvI1tKWuR4Bzh2Enme03OtORz8aDSVep1GyHx/9LNyNh4/AoGAcpdk/nIB8ENrMTVrZAYsJ+OaqlIhTnla4U/EmPVtkPCFaaZmTHUS3ZfUcpcPjXFZv5CVteDlWnD1EiJRP3/epmnFiMw5qKeKGpAquSo1LhEpagu/2aGel8EcvK0ad2Mk8XlvXuz2dbads/eCzluCFdAESCW+BYdWDbNPGJClP8kCgYBoT+0res0efi1cn6H0fPx/q33Wmgf47txVrzQN0ZEWDFOOhnErvGpRan9AG+LGvp7TvWWHnW13qjFCXGocWcbaoqsLabkov961R8ij2MTeToz6V+7YfK0KBt/h2HHJ5t/CybNxE5iYFyUMI7GTlC2GzFrnvxH/UYwUma1AplkpEw==';
      final key = decodeRsaPrivKey(decode64(encodeAscii(keyStr))!);
      final encKey = decodeAscii(encode64(encodeRsaPrivKey(key)));
      expect(encKey, keyStr);
    });
  });
}
