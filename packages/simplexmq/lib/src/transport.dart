import 'dart:async';
import 'dart:typed_data';
import 'package:pointycastle/asymmetric/api.dart';
import 'buffer.dart';
import 'crypto.dart';
import 'rsa_keys.dart';

abstract class Transport {
  Future<Uint8List> read(int n);
  Future<void> write(Uint8List data);
  Future<void> close();
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

class SMPServer {
  final String host;
  final int? port;
  final Uint8List? keyHash;
  SMPServer(this.host, [this.port, this.keyHash]);
}

class SessionKey {
  final AESKey aesKey;
  final Uint8List baseIV;
  int _counter = 0;
  SessionKey._(this.aesKey, this.baseIV);
  static SessionKey create() => SessionKey._(AESKey.random(), randomIV());
  Uint8List serialize() => concat(aesKey.bytes, baseIV);
}

class ServerHeader {
  final int blockSize;
  final int keySize;
  ServerHeader(this.blockSize, this.keySize);
}

class ServerHandshake {
  final RSAPublicKey publicKey;
  final int blockSize;
  ServerHandshake(this.publicKey, this.blockSize);
}

typedef SMPVersion = List<int>;

// SMPVersion _currentSMPVersion = const [0, 4, 1, 0];
const int _serverHeaderSize = 8;
const int _binaryRsaTransport = 0;
const int _transportBlockSize = 4096;
const int _maxTransportBlockSize = 65536;

class SMPTransportClient {
  final Transport _conn;
  final _sndKey = SessionKey.create();
  final _rcvKey = SessionKey.create();
  final int blockSize;
  SMPTransportClient._(this._conn, this.blockSize);

  static Future<SMPTransportClient> connect(Transport conn,
      {Uint8List? keyHash, int? blockSize}) {
    return _clientHandshake(conn, keyHash, blockSize);
  }

  Future<void> close() {
    return _conn.close();
  }

  static Future<SMPTransportClient> _clientHandshake(
      Transport conn, Uint8List? keyHash, int? blkSize) async {
    final srv = await _getHeaderAndPublicKey_1_2(conn, keyHash);
    final t = SMPTransportClient._(conn, blkSize ?? srv.blockSize);
    await t._sendEncryptedKeys_4(srv.publicKey);
    _checkVersion(await t._getWelcome_6());
    return t;
  }

  static Future<ServerHandshake> _getHeaderAndPublicKey_1_2(
      Transport conn, Uint8List? keyHash) async {
    final srvHeader = parseServerHeader(await conn.read(_serverHeaderSize));
    final blkSize = srvHeader.blockSize;
    if (blkSize < _transportBlockSize || blkSize > _maxTransportBlockSize) {
      throw Exception('smp handshake header error: bad block size $blkSize');
    }
    final rawKey = await conn.read(srvHeader.keySize);
    if (keyHash != null) validateKeyHash_2(rawKey, keyHash);
    final serverKey = decodeRsaPubKey(rawKey);
    return ServerHandshake(serverKey, blkSize);
  }

  static void validateKeyHash_2(Uint8List rawKey, Uint8List keyHash) {
    // todo
  }

  static ServerHeader parseServerHeader(Uint8List a) {
    if (a.length != 8) {
      throw Exception('smp handshake error: bad header size ${a.length}');
    }
    final v = ByteData.sublistView(a);
    final blockSize = v.getUint32(0);
    final transportMode = v.getUint16(4);
    if (transportMode != _binaryRsaTransport) {
      throw Exception('smp handshake error: bad transport mode $transportMode');
    }
    final keySize = v.getUint16(6);
    return ServerHeader(blockSize, keySize);
  }

  Future<void> _sendEncryptedKeys_4(RSAPublicKey serverKey) async {
    final header = encryptOAEP(serverKey, _clientHeader());
    await _conn.write(header);
  }

  Uint8List _clientHeader() => concatN([
        encodeInt32(blockSize),
        encodeInt16(_binaryRsaTransport),
        _sndKey.serialize(),
        _rcvKey.serialize()
      ]);

  Future<SMPVersion> _getWelcome_6() async =>
      _parseSMPVersion(await _readEncrypted());

  static SMPVersion _parseSMPVersion(Uint8List block) {
    return [];
  }

  static void _checkVersion(SMPVersion version) {}

  Future<Uint8List> _readEncrypted() async {
    final block = await _conn.read(blockSize);
    final iv = _nextIV(_rcvKey);
    return decryptAES(_rcvKey.aesKey, iv, block);
  }

  static Uint8List _nextIV(SessionKey sk) {
    final c = encodeInt32(sk._counter++);
    final start = sk.baseIV.sublist(0, 4);
    final rest = sk.baseIV.sublist(4);
    for (int i = 0; i < 4; i++) {
      start[i] ^= c[i];
    }
    return concat(start, rest);
  }
}
