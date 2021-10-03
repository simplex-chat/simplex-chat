import "dart:typed_data";
import "package:simplexmq/src/buffer.dart";
import "package:test/test.dart";

final hello123 = Uint8List.fromList([104, 101, 108, 108, 111, 49, 50, 51]);

class Base64Test {
  final String description;
  final String binary;
  final String base64;

  Base64Test(this.binary, this.base64) : description = binary;
  Base64Test.withDescription(this.description, this.binary, this.base64);
}

void main() {
  group("ascii encoding/decoding", () {
    test("encodeAscii", () {
      expect(encodeAscii("hello123"), hello123);
    });

    test("decodeAscii", () {
      expect(decodeAscii(hello123), "hello123");
    });
  });

  group("base-64 encoding/decoding", () {
    String allBinaryChars() {
      final a = Uint8List(256);
      for (var i = 0; i < 256; i++) {
        a[i] = i;
      }
      return decodeAscii(a);
    }

    final base64tests = [
      Base64Test("\x12\x34\x56\x78", "EjRWeA=="),
      Base64Test("hello123", "aGVsbG8xMjM="),
      Base64Test("Hello world", "SGVsbG8gd29ybGQ="),
      Base64Test("Hello worlds!", "SGVsbG8gd29ybGRzIQ=="),
      Base64Test("May", "TWF5"),
      Base64Test("Ma", "TWE="),
      Base64Test("M", "TQ=="),
      Base64Test.withDescription(
        "all binary chars",
        allBinaryChars(),
        "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==",
      ),
    ];

    test("encode64", () {
      for (final t in base64tests) {
        expect(encode64(encodeAscii(t.binary)), encodeAscii(t.base64));
      }
    });

    test("decode64", () {
      for (final t in base64tests) {
        expect(decode64(encodeAscii(t.base64)), encodeAscii(t.binary));
      }
      expect(decode64(encodeAscii("TWE")), null);
      expect(decode64(encodeAscii("TWE==")), null);
      expect(decode64(encodeAscii("TW!=")), null);
    });
  });
}
