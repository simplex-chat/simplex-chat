import "dart:async";
import "dart:typed_data";

abstract class Transport {
  Future<Uint8List> read(int n);
  Future<void> write(Uint8List data);
}

Stream<Uint8List> blockStream(Transport t, int blockSize) async* {
  try {
    while (true) {
      yield await t.read(blockSize);
    }
  } catch (e) {
    return;
  }
}
