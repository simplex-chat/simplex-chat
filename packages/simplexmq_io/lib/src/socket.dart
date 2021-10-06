import "dart:async";
import "dart:collection";
import "dart:io";
import "dart:typed_data";

abstract class Transport {
  Future<Uint8List> read(int n);
  Future<void> write(Uint8List data);
}

class _STReaders {
  final Completer<Uint8List> completer = Completer();
  final int size;
  _STReaders(this.size);
}

class SocketTransport implements Transport {
  final Socket _socket;
  late final StreamSubscription _subscription;
  final Duration _timeout;
  final int _bufferSize;
  Uint8List _buffer = Uint8List(0);
  final ListQueue<_STReaders> _readers = ListQueue(16);
  SocketTransport._new(this._socket, this._timeout, this._bufferSize);

  static Future<SocketTransport> connect(String host, int port,
      {Duration timeout = const Duration(seconds: 1),
      int bufferSize = 16384}) async {
    final socket = await Socket.connect(host, port, timeout: timeout);
    final t = SocketTransport._new(socket, timeout, bufferSize);
    final subscription =
        socket.listen(t._onData, onError: t._finalize, onDone: t._finalize);
    t._subscription = subscription;
    return t;
  }

  @override
  Future<Uint8List> read(int n) async {
    if (_readers.isEmpty && _buffer.length >= n) {
      final data = _buffer.sublist(0, n);
      _buffer = _buffer.sublist(n);
      return Future.value(data);
    }
    final r = _STReaders(n);
    _readers.add(r);
    return r.completer.future;
  }

  @override
  Future<void> write(Uint8List data) {
    _socket.add(data);
    return _socket.flush();
  }

  void _onData(Uint8List data) {
    _buffer.addAll(data);
    while (_readers.isNotEmpty && _readers.first.size <= _buffer.length) {
      final r = _readers.removeFirst();
      final d = _buffer.sublist(0, r.size);
      r.completer.complete(d);
      _buffer = _buffer.sublist(r.size);
    }
    final overflow = _buffer.length >= _bufferSize;
    if (_subscription.isPaused && !overflow) {
      _subscription.resume();
    } else if (!_subscription.isPaused && overflow) {
      _subscription.pause();
    }
  }

  void _finalize() {
    _subscription.cancel();
    _socket.destroy();
    while (_readers.isNotEmpty) {
      final r = _readers.removeFirst();
      r.completer.completeError(Exception("socket closed"));
    }
  }
}
