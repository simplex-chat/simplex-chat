import 'dart:async';
import 'dart:typed_data';
import 'package:pointycastle/asymmetric/api.dart';
import 'package:pointycastle/digests/sha256.dart';
import 'buffer.dart';
import 'crypto.dart';
import 'parser.dart';
import 'protocol.dart';
import 'rsa_keys.dart';

abstract class Transport {
  Future<Uint8List> read(int n);
  Future<void> write(Uint8List data);
  Future<void> close();
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

SMPVersion _currentSMPVersion = const [0, 4, 1, 0];
const int _serverHeaderSize = 8;
const int _binaryRsaTransport = 0;
const int _transportBlockSize = 4096;
const int _maxTransportBlockSize = 65536;

class _Request {
  final Uint8List queueId;
  final Completer<BrokerResponse> completer = Completer();
  _Request(this.queueId);
}

class Either<L, R> {
  final L? left;
  final R? right;
  Either.left(L this.left) : right = null;
  Either.right(R this.right) : left = null;
}

enum ClientErrorType { SMPServerError, SMPResponseError, SMPUnexpectedResponse }

class BrokerResponse {
  final BrokerCommand? command;
  final ClientErrorType? errorType;
  final ERR? error;
  BrokerResponse(BrokerCommand this.command)
      : errorType = null,
        error = null;
  BrokerResponse.error(ClientErrorType this.errorType, this.error)
      : command = null;
}

class ClientTransmission {
  final String corrId;
  final Uint8List queueId;
  final ClientCommand command;
  ClientTransmission(this.corrId, this.queueId, this.command);

  Uint8List serialize() =>
      unwordsN([encodeAscii(corrId), encode64(queueId), command.serialize()]);
}

class BrokerTransmission {
  final String corrId;
  final Uint8List queueId;
  final BrokerCommand? command;
  final ERR? error;
  BrokerTransmission(this.corrId, this.queueId, BrokerCommand this.command)
      : error = null;
  BrokerTransmission.error(this.corrId, this.queueId, ERR this.error)
      : command = null;
}

final badBlock = BrokerTransmission.error('', empty, ERR(ErrorType.BLOCK));

class SMPTransportClient {
  final Transport _conn;
  final _sndKey = SessionKey.create();
  final _rcvKey = SessionKey.create();
  final int blockSize;
  int _corrId = 0;
  bool _messageStreamCreated = false;
  final Map<String, _Request> _sentCommands = {};
  SMPTransportClient._(this._conn, this.blockSize);

  static Future<SMPTransportClient> connect(Transport conn,
      {Uint8List? keyHash, int? blockSize}) {
    return _clientHandshake(conn, keyHash, blockSize);
  }

  Future<void> close() {
    return _conn.close();
  }

  Future<BrokerResponse> sendSMPCommand(
      RSAPrivateKey? key, Uint8List queueId, ClientCommand cmd) async {
    final corrId = (++_corrId).toString();
    final t = ClientTransmission(corrId, queueId, cmd).serialize();
    final sig = key == null ? empty : encode64(signPSS(key, t));
    final data = unwordsN([sig, t, empty]);
    final r = _sentCommands[corrId] = _Request(queueId);
    await _writeEncrypted(data);
    return r.completer.future;
  }

  Stream<BrokerTransmission> messageStream() {
    if (_messageStreamCreated) {
      throw Exception('message stream already created');
    }
    _messageStreamCreated = true;
    return _messageStream();
  }

  Stream<BrokerTransmission> _messageStream() async* {
    try {
      while (true) {
        final block = await _readEncrypted();
        final t = _parseBrokerTransmission(block);
        if (t.corrId == '') {
          yield t;
        } else {
          final r = _sentCommands.remove(t.corrId);
          if (r == null) {
            yield t;
          } else {
            final cmd = t.command;
            r.completer.complete(r.queueId.equal(t.queueId)
                ? cmd == null
                    ? BrokerResponse.error(
                        ClientErrorType.SMPResponseError, t.error)
                    : cmd is ERR
                        ? BrokerResponse.error(
                            ClientErrorType.SMPServerError, cmd)
                        : BrokerResponse(cmd)
                : BrokerResponse.error(
                    ClientErrorType.SMPUnexpectedResponse, null));
          }
        }
      }
    } catch (e) {
      return;
    }
  }

  static BrokerTransmission _parseBrokerTransmission(Uint8List s) {
    final p = Parser(s);
    p.space();
    final cId = p.word();
    p.space();
    final queueId = p.tryParse((p) => p.base64()) ?? empty;
    p.space();
    if (p.fail || cId == null) return badBlock;
    final corrId = decodeAscii(cId);
    final command = smpCommandP(p);
    if (command == null) {
      return BrokerTransmission.error(
          corrId, queueId, ERR.cmd(CmdErrorType.SYNTAX));
    }
    if (command is! BrokerCommand) {
      return BrokerTransmission.error(
          corrId, queueId, ERR.cmd(CmdErrorType.PROHIBITED));
    }
    final qErr = _tQueueError(queueId, command);
    if (qErr != null) {
      return BrokerTransmission.error(corrId, queueId, ERR.cmd(qErr));
    }
    return BrokerTransmission(corrId, queueId, command);
  }

  static CmdErrorType? _tQueueError(Uint8List queueId, BrokerCommand cmd) {
    if (cmd is IDS || cmd is PONG) {
      if (queueId.isNotEmpty) return CmdErrorType.HAS_AUTH;
    } else if (cmd is! ERR && queueId.isEmpty) {
      return CmdErrorType.NO_QUEUE;
    }
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
    if (keyHash.equal(SHA256Digest().process(rawKey))) return;
    throw Exception('smp handshake error: bad key hash');
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
    final p = Parser(block);
    final SMPVersion version = [0, 0, 0, 0];
    void setVer(int i, int? v) {
      if (v == null || p.fail) {
        throw Exception('smp handshake error: bad version format');
      }
      version[i] = v;
    }

    for (var i = 0; i < 3; i++) {
      final v = p.decimal();
      p.char(charDot);
      setVer(i, v);
    }
    final v = p.decimal();
    p.space();
    setVer(3, v);
    return version;
  }

  static void _checkVersion(SMPVersion srvVersion) {
    final s0 = srvVersion[0];
    final c0 = _currentSMPVersion[0];
    if (s0 > c0 || (s0 == c0 && srvVersion[1] > _currentSMPVersion[1])) {
      throw Exception('smp handshake error: incompatible server version');
    }
  }

  Future<Uint8List> _readEncrypted() async {
    final block = await _conn.read(blockSize);
    final iv = _nextIV(_rcvKey);
    return decryptAES(_rcvKey.aesKey, iv, block);
  }

  Future<void> _writeEncrypted(Uint8List data) {
    final iv = _nextIV(_sndKey);
    final block = encryptAES(_sndKey.aesKey, iv, blockSize - 16, data);
    return _conn.write(block);
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
