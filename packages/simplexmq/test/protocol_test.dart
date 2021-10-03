import "dart:typed_data";
import "package:simplexmq/src/buffer.dart";
import "package:simplexmq/src/protocol.dart";
import "package:test/test.dart";

void main() {
  group("Parsing & serializing SMP commands", () {
    group("valid commands", () {
      Null Function() parseSerialize(SMPCommand cmd) => () {
            final s = cmd.serialize();
            expect(parseSMPCommand(s)?.serialize(), s);
            expect(parseSMPCommand(concat(s, Uint8List.fromList([charSpace]))),
                null);
          };

      test("NEW", parseSerialize(NEW(encodeAscii("rsa:1234"))));
      test("SUB", parseSerialize(SUB()));
      test("KEY", parseSerialize(KEY(encodeAscii("rsa:1234"))));
      test("ACK", parseSerialize(ACK()));
      test("OFF", parseSerialize(OFF()));
      test("DEL", parseSerialize(DEL()));
      test("SEND", parseSerialize(SEND(encodeAscii("hello"))));
      test("PING", parseSerialize(PING()));
      test("IDS", parseSerialize(IDS(encodeAscii("abc"), encodeAscii("def"))));
      test(
          "MSG",
          parseSerialize(MSG(encodeAscii("fgh"), DateTime.now().toUtc(),
              encodeAscii("hello"))));
      test("END", parseSerialize(END()));
      test("OK", parseSerialize(OK()));
      test("ERR", parseSerialize(ERR(ErrorType.AUTH)));
      test("ERR CMD", parseSerialize(ERR.cmd(CmdErrorType.SYNTAX)));
      test("PONG", parseSerialize(PONG()));
    });

    group("invalid commands", () {
      void Function() parseFailure(String s) =>
          () => expect(parseSMPCommand(encodeAscii(s)), null);

      void Function() parseSuccess(String s) =>
          () => expect(parseSMPCommand(encodeAscii(s)) is SMPCommand, true);

      group("NEW", () {
        test("ok", parseSuccess("NEW rsa:abcd"));
        test("no key", parseFailure("NEW"));
        test("invalid base64", parseFailure("NEW rsa:abc"));
        test("double space", parseFailure("NEW  rsa:abcd"));
      });

      group("KEY", () {
        test("ok", parseSuccess("KEY rsa:abcd"));
        test("no key", parseFailure("KEY"));
        test("invalid base64", parseFailure("KEY rsa:abc"));
        test("double space", parseFailure("KEY  rsa:abcd"));
      });

      group("SEND", () {
        test("ok", parseSuccess("SEND 5 hello "));
        test("no size", parseFailure("SEND hello "));
        test("incorrect size", parseFailure("SEND 6 hello "));
        test("no trailing space", parseFailure("SEND 5 hello"));
        test("double space 1", parseFailure("SEND  5 hello "));
        test("double space 2", parseFailure("SEND 5  hello "));
      });

      group("IDS", () {
        test("ok", parseSuccess("IDS abcd efgh"));
        test("no IDs", parseFailure("IDS"));
        test("only one ID", parseFailure("IDS abcd"));
        test("invalid base64 1", parseFailure("IDS abc efgh"));
        test("invalid base64 2", parseFailure("IDS abcd efg"));
        test("double space 1", parseFailure("IDS  abcd efgh"));
        test("double space 2", parseFailure("IDS abcd  efgh"));
      });

      group("MSG", () {
        final String ts = "2021-10-03T10:50:59.895Z";
        test("ok", parseSuccess("MSG abcd $ts 5 hello "));
        test("invalid base64", parseFailure("MSG abc $ts 5 hello "));
        test("invalid timestamp 1",
            parseFailure("MSG abc 2021-10-03T10:50:59.895 5 hello "));
        test("invalid timestamp 2",
            parseFailure("MSG abc 2021-14-03T10:50:59.895Z 5 hello "));
        test("no size", parseFailure("MSG abcd $ts hello "));
        test("incorrect size", parseFailure("MSG abcd $ts 6 hello "));
        test("no trailing space", parseFailure("MSG abcd $ts 5 hello"));
        test("double space 1", parseFailure("MSG  abcd $ts 5 hello "));
        test("double space 2", parseFailure("MSG abcd  $ts 5 hello "));
        test("double space 3", parseFailure("MSG abcd $ts  5 hello "));
        test("double space 4", parseFailure("MSG abcd $ts 5  hello "));
      });

      group("ERR", () {
        test("ok 1", parseSuccess("ERR AUTH"));
        test("ok 2", parseSuccess("ERR CMD SYNTAX"));
        test("unknown error", parseFailure("ERR HELLO"));
        test("unknown CMD error", parseFailure("ERR CMD HELLO"));
        test("bad sub-error", parseFailure("ERR AUTH SYNTAX"));
        test("double space 1", parseFailure("ERR  AUTH"));
        test("double space 2", parseFailure("ERR  CMD SYNTAX"));
        test("double space 3", parseFailure("ERR CMD  SYNTAX"));
      });
    });
  });
}
