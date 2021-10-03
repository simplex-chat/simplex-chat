import "dart:typed_data";
import "buffer.dart";

typedef BinaryTags<T> = Map<T, Uint8List>;

int cc(String c) => c.codeUnitAt(0);

final char_0 = cc("0");
final char_9 = cc("9");
final char_a = cc("a");
final char_z = cc("z");
final char_A = cc("A");
final char_Z = cc("Z");
final char_plus = cc("+");
final char_slash = cc("/");

class Parser {
  final Uint8List _s;
  int _pos = 0;
  bool _fail = false;
  Parser(this._s);

  bool get fail => _fail;

  // only calls `parse` if the parser did not previously fail
  T? _run<T>(T? Function() parse) {
    if (_fail || _pos >= _s.length) {
      _fail = true;
      return null;
    }
    final res = parse();
    if (res == null) _fail = true;
    return res;
  }

  // takes a required number of bytes
  Uint8List? take(int len) => _run(() {
        final end = _pos + len;
        if (end > _s.length) return null;
        final res = _s.sublist(_pos, end);
        _pos = end;
        return res;
      });

  // takes chars (> 0) while condition is true; function isAlphaNum or isDigit can be used
  Uint8List? takeWhile1(bool Function(int) f) => _run(() {
        final pos = _pos;
        while (f(_s[_pos])) {
          _pos++;
        }
        return _pos > pos ? _s.sublist(pos, _pos) : null;
      });

  // takes the non-empty word until the first space or until the end of the string
  Uint8List? word() => _run(() {
        int pos = _s.indexOf(char_space, _pos);
        if (pos == -1) pos = _s.length;
        if (pos <= _pos) return null;
        final res = _s.sublist(_pos, pos);
        _pos = pos;
        return res;
      });

  bool? str(Uint8List s) => _run(() {
        for (int i = 0, j = _pos; i < s.length; i++, j++) {
          if (s[i] != _s[j]) return null;
        }
        _pos += s.length;
        return true;
      });

  // takes space
  bool? space() => _run(() {
        if (_s[_pos] == char_space) {
          _pos++;
          return true;
        }
      });

  int? decimal() => _run(() {
        final s = takeWhile1(isDigit);
        if (s == null) return null;
        int n = 0;
        for (int i = 0; i < s.length; i++) {
          n *= 10;
          n += s[i] - char_0;
        }
        return n;
      });

  DateTime? datetime() => _run(() {
        final s = word();
        if (s != null) return DateTime.tryParse(decodeAscii(s));
      });

  // takes base-64 encoded string and returns decoded binary
  Uint8List? base64() => _run(() {
        bool tryCharEqual() {
          final ok = _pos < _s.length && _s[_pos] == char_equal;
          if (ok) _pos++;
          return ok;
        }

        final pos = _pos;
        int c;
        do {
          c = _s[_pos];
        } while ((isAlphaNum(c) || c == char_plus || c == char_slash) &&
            ++_pos < _s.length);

        if (tryCharEqual()) tryCharEqual();
        return _pos > pos ? decode64(_s.sublist(pos, _pos)) : null;
      });

  // takes one of the binary tags and returns its key
  T? someStr<T>(BinaryTags<T> ts) => _run(() {
        outer:
        for (final t in ts.entries) {
          final s = t.value;
          for (int i = 0, j = _pos; i < s.length; i++, j++) {
            if (s[i] != _s[j]) continue outer;
          }
          _pos += s.length;
          return t.key;
        }
        return null;
      });

  bool get end => _pos >= _s.length;
}

bool isDigit(int c) => c >= char_0 && c <= char_9;

bool isAlphaNum(int c) =>
    (c >= char_0 && c <= char_9) ||
    (c >= char_a && c <= char_z) ||
    (c >= char_A && c <= char_Z);
