// import 'dart:io';
import 'package:simplexmq/simplexmq.dart';
import 'package:simplexmq_io/simplexmq_io.dart';
import 'package:test/test.dart';

void main() {
  group('SMP transport', () {
    test(
      'establish connection (expects SMP server on localhost:5223)',
      () async {
        final conn = await SocketTransport.connect('localhost', 5223);
        final smp = await SMPTransportClient.connect(conn);
        expect(smp is SMPTransportClient, true);
      },
      skip: 'requires SMP server on port 5223',
    );
  });
}
