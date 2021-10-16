import 'package:simplexmq/simplexmq.dart';
import 'package:simplexmq_io/simplexmq_io.dart';
import 'package:test/test.dart';

void main() {
  group('SMP transport', () {
    test('establish connection (expects SMP server on localhost:5423)',
        () async {
      final conn = await SocketTransport.connect('localhost', 5423);
      final smp = await SMPTransportClient.connect(conn);
      print('connected');
      print(smp);
    }, skip: 'socket does not connect');
  });
}
