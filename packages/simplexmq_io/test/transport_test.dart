import 'package:simplexmq/simplexmq.dart';
import 'package:simplexmq/src/buffer.dart';
import 'package:simplexmq/src/crypto.dart';
import 'package:simplexmq/src/rsa_keys.dart';
import 'package:simplexmq_io/simplexmq_io.dart';
import 'package:test/test.dart';

final keyHash =
    decode64(encodeAscii('pH7bg7B6vB3uJ1poKmClTAqr7yYWnAtapnIDN7ypKxU='));
void main() {
  group('SMP transport (expects SMP server on localhost:5223)', () {
    test('establish connection', () async {
      final conn = await SocketTransport.connect('localhost', 5223);
      final smp = await SMPTransportClient.connect(conn, keyHash: keyHash);
      expect(smp is SMPTransportClient, true);
    });

    test('should create SMP queue and send message', () async {
      final conn1 = await SocketTransport.connect('localhost', 5223);
      final alice = await SMPTransportClient.connect(conn1, keyHash: keyHash);
      final aliceKeys = generateRSAkeyPair();
      final rcvKeyStr = encode64(encodeRsaPubKey(aliceKeys.publicKey));

      // final conn2 = await SocketTransport.connect('localhost', 5223);
      // final bob = await SMPTransportClient.connect(conn2, keyHash: keyHash);
      // final bobKeys = generateRSAkeyPair();
      // final sndKeyStr = encode64(encodeRsaPubKey(bobKeys.publicKey));

      // print('we are here');

      final resp = await alice.sendSMPCommand(
          aliceKeys.privateKey, empty, NEW(rcvKeyStr));
      print(resp);
    });
    // });
  }, skip: 'requires SMP server on port 5223');
}
