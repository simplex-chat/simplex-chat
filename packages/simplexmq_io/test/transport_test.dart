// import 'dart:io';
import 'package:simplexmq/simplexmq.dart';
import 'package:simplexmq_io/simplexmq_io.dart';
import 'package:test/test.dart';

void main() {
  group('SMP transport', () {
    test('establish connection (expects SMP server on localhost:5223)',
        () async {
      // await Socket.connect('localhost', 5223);
      final conn = await SocketTransport.connect('localhost', 5223);
      final smp = await SMPTransportClient.connect(conn);
      print('connected');
      print(smp);
    }, skip: 'requires server; pc OAEP impl is not compatible as it uses SHA1');
  });
}
