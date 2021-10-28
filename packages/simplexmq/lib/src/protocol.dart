import 'dart:typed_data';
import 'buffer.dart';
import 'parser.dart';

abstract class SMPCommand {
  Uint8List serialize();
}

abstract class ClientCommand extends SMPCommand {}

abstract class BrokerCommand extends SMPCommand {}

final rsaPrefix = encodeAscii('rsa:');

Uint8List serializePubKey(Uint8List rcvPubKey) =>
    concat(rsaPrefix, encode64(rcvPubKey));

final Uint8List cNEW = encodeAscii('NEW');
final Uint8List cSUB = encodeAscii('SUB');
final Uint8List cKEY = encodeAscii('KEY');
final Uint8List cACK = encodeAscii('ACK');
final Uint8List cOFF = encodeAscii('OFF');
final Uint8List cDEL = encodeAscii('DEL');
final Uint8List cSEND = encodeAscii('SEND');
final Uint8List cPING = encodeAscii('PING');
final Uint8List cIDS = encodeAscii('IDS');
final Uint8List cMSG = encodeAscii('MSG');
final Uint8List cEND = encodeAscii('END');
final Uint8List cOK = encodeAscii('OK');
final Uint8List cERR = encodeAscii('ERR');
final Uint8List cPONG = encodeAscii('PONG');

enum SMPCmdTag {
  NEW,
  SUB,
  KEY,
  ACK,
  OFF,
  DEL,
  SEND,
  PING,
  IDS,
  MSG,
  END,
  OK,
  ERR,
  PONG,
}

final BinaryTags<SMPCmdTag> smpCmdTags = {
  SMPCmdTag.NEW: cNEW,
  SMPCmdTag.SUB: cSUB,
  SMPCmdTag.KEY: cKEY,
  SMPCmdTag.ACK: cACK,
  SMPCmdTag.OFF: cOFF,
  SMPCmdTag.DEL: cDEL,
  SMPCmdTag.SEND: cSEND,
  SMPCmdTag.PING: cPING,
  SMPCmdTag.IDS: cIDS,
  SMPCmdTag.MSG: cMSG,
  SMPCmdTag.END: cEND,
  SMPCmdTag.OK: cOK,
  SMPCmdTag.ERR: cERR,
  SMPCmdTag.PONG: cPONG,
};

class NEW extends ClientCommand {
  final Uint8List rcvPubKey;
  NEW(this.rcvPubKey);
  @override
  Uint8List serialize() => unwords(cNEW, serializePubKey(rcvPubKey));
}

class SUB extends ClientCommand {
  @override
  Uint8List serialize() => cSUB;
}

class KEY extends ClientCommand {
  final Uint8List sndPubKey;
  KEY(this.sndPubKey);
  @override
  Uint8List serialize() => unwords(cKEY, serializePubKey(sndPubKey));
}

class ACK extends ClientCommand {
  @override
  Uint8List serialize() => cACK;
}

class OFF extends ClientCommand {
  @override
  Uint8List serialize() => cOFF;
}

class DEL extends ClientCommand {
  @override
  Uint8List serialize() => cDEL;
}

List<Uint8List> serializeMsg(Uint8List msg) =>
    [encodeAscii(msg.length.toString()), msg, empty];

class SEND extends ClientCommand {
  final Uint8List msgBody;
  SEND(this.msgBody);
  @override
  Uint8List serialize() => unwordsN([cSEND, ...serializeMsg(msgBody)]);
}

class PING extends ClientCommand {
  @override
  Uint8List serialize() => cPING;
}

class IDS extends BrokerCommand {
  final Uint8List rcvId;
  final Uint8List sndId;
  IDS(this.rcvId, this.sndId) : super();
  @override
  Uint8List serialize() => unwordsN([cIDS, encode64(rcvId), encode64(sndId)]);
}

class MSG extends BrokerCommand {
  final Uint8List msgId;
  final DateTime ts;
  final Uint8List msgBody;
  MSG(this.msgId, this.ts, this.msgBody);
  @override
  Uint8List serialize() => unwordsN([
        cMSG,
        encode64(msgId),
        encodeAscii(ts.toIso8601String()),
        ...serializeMsg(msgBody)
      ]);
}

class END extends BrokerCommand {
  @override
  Uint8List serialize() => cEND;
}

class OK extends BrokerCommand {
  @override
  Uint8List serialize() => cOK;
}

enum ErrorType { BLOCK, CMD, AUTH, QUOTA, NO_MSG, INTERNAL }

final BinaryTags<ErrorType> errorTags = {
  ErrorType.BLOCK: encodeAscii('BLOCK'),
  ErrorType.CMD: encodeAscii('CMD'),
  ErrorType.AUTH: encodeAscii('AUTH'),
  ErrorType.QUOTA: encodeAscii('QUOTA'),
  ErrorType.NO_MSG: encodeAscii('NO_MSG'),
  ErrorType.INTERNAL: encodeAscii('INTERNAL'),
};

enum CmdErrorType { PROHIBITED, KEY_SIZE, SYNTAX, NO_AUTH, HAS_AUTH, NO_QUEUE }

final BinaryTags<CmdErrorType> cmdErrorTags = {
  CmdErrorType.PROHIBITED: encodeAscii('PROHIBITED'),
  CmdErrorType.KEY_SIZE: encodeAscii('KEY_SIZE'),
  CmdErrorType.SYNTAX: encodeAscii('SYNTAX'),
  CmdErrorType.NO_AUTH: encodeAscii('NO_AUTH'),
  CmdErrorType.HAS_AUTH: encodeAscii('HAS_AUTH'),
  CmdErrorType.NO_QUEUE: encodeAscii('NO_QUEUE'),
};

class ERR extends BrokerCommand {
  final ErrorType err;
  final CmdErrorType? cmdErr;
  ERR(this.err)
      : cmdErr = err == ErrorType.CMD
            ? throw ArgumentError('CMD error should be created with ERR.CMD')
            : null;
  ERR.cmd(CmdErrorType this.cmdErr) : err = ErrorType.CMD;
  @override
  Uint8List serialize() {
    final _err = errorTags[err]!;
    return cmdErr == null
        ? unwords(cERR, _err)
        : unwordsN([cERR, _err, cmdErrorTags[cmdErr!]!]);
  }
}

class PONG extends BrokerCommand {
  @override
  Uint8List serialize() => cPONG;
}

final Map<SMPCmdTag, SMPCommand? Function(Parser p)> smpCmdParsers = {
  SMPCmdTag.NEW: (p) {
    p.space();
    final key = pubKeyP(p);
    if (key != null) return NEW(key);
  },
  SMPCmdTag.SUB: (_) => SUB(),
  SMPCmdTag.KEY: (p) {
    p.space();
    final key = pubKeyP(p);
    if (key != null) return KEY(key);
  },
  SMPCmdTag.ACK: (_) => ACK(),
  SMPCmdTag.OFF: (_) => OFF(),
  SMPCmdTag.DEL: (_) => DEL(),
  SMPCmdTag.SEND: (p) {
    p.space();
    final msg = messageP(p);
    if (msg != null) return SEND(msg);
  },
  SMPCmdTag.PING: (_) => PING(),
  SMPCmdTag.IDS: (p) {
    p.space();
    final rId = p.base64();
    p.space();
    final sId = p.base64();
    if (rId != null && sId != null) return IDS(rId, sId);
  },
  SMPCmdTag.MSG: (p) {
    p.space();
    final msgId = p.base64();
    p.space();
    final ts = p.datetime();
    p.space();
    final msg = messageP(p);
    if (msgId != null && ts != null && msg != null) return MSG(msgId, ts, msg);
  },
  SMPCmdTag.END: (_) => END(),
  SMPCmdTag.OK: (_) => OK(),
  SMPCmdTag.ERR: (p) {
    p.space();
    final err = p.someStr(errorTags);
    if (err == ErrorType.CMD) {
      p.space();
      final cmdErr = p.someStr(cmdErrorTags);
      if (cmdErr != null) return ERR.cmd(cmdErr);
    } else if (err != null) {
      return ERR(err);
    }
  },
  SMPCmdTag.PONG: (_) => PONG(),
};

SMPCommand? smpCommandP(Parser p) {
  final cmd = p.someStr(smpCmdTags);
  return p.fail ? null : smpCmdParsers[cmd]!(p);
}

SMPCommand? parseSMPCommand(Uint8List s) {
  final p = Parser(s);
  final cmd = smpCommandP(p);
  if (cmd != null && p.end) return cmd;
}

Uint8List? pubKeyP(Parser p) {
  p.str(rsaPrefix);
  return p.base64();
}

Uint8List? messageP(Parser p) {
  final len = p.decimal();
  p.space();
  Uint8List? msg;
  if (len != null) msg = p.take(len);
  p.space();
  return p.fail ? null : msg;
}
