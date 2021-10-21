// ignore_for_file: prefer_double_quotes

import 'dart:async';
import 'dart:io';
import 'dart:typed_data';

import 'package:simplexmq_io/simplexmq_io.dart';
import 'package:test/test.dart';

const localhost = 'localhost';
void main() {
  group('transport', () {
    Future<ServerSocket> startServer(
        void Function(Socket client) handleConnection) async {
      var server = await ServerSocket.bind(localhost, 8080);
      server.listen(handleConnection);
      return server;
    }

    test('simple write', () async {
      var completer = Completer<Uint8List>();

      var server = await startServer((Socket client) {
        client.listen(
          (Uint8List data) async {
            completer.complete(data);
          },
        );
      });
      var transport = await SocketTransport.connect(localhost, 8080);
      await transport.write(Uint8List.fromList([1, 2, 3]));

      expect(await completer.future, [1, 2, 3]);
      await transport.close();
      await server.close();
    });

    test('simple read', () async {
      var server = await startServer((Socket client) {
        client.add(Uint8List.fromList([1, 2, 3]));
      });
      var transport = await SocketTransport.connect(localhost, server.port);
      expect(await transport.read(3), [1, 2, 3]);
      await transport.close();
      await server.close();
    });
  });
}
