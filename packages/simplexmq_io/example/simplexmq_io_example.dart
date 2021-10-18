import 'dart:io';

void main() {
  ServerSocket.bind('localhost', 8080)
      .then((server) => server.listen((Socket socket) {
            print('New client connection');
            socket.listen((List<int> data) {
              String result = String.fromCharCodes(data);
              print(result);
            });
          }));
}
