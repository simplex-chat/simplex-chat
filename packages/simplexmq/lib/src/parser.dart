import 'dart:typed_data';
import 'buffer.dart';

typedef BinaryTags<T> = Map<T, Uint8List>;

int cc(String c) => c.codeUnitAt(0);

final char0 = cc('0');
final char9 = cc('9');
final charLowerA = cc('a');
final charLowerZ = cc('z');
final charUpperA = cc('A');
final charUpperZ = cc('Z');
final charPlus = cc('+');
final charSlash = cc('/');
final charDot = cc('.');

class Parser {
  final Uint8List _s;
  final List<int> _positions = [];
  int _pos = 0;
  bool _fail = false;
  Parser(this._s);

  bool get fail => _fail;
  bool get end => _pos >= _s.length;

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

  T? tryParse<T>(T? Function(Parser p) parse) {
    if (_fail || _pos >= _s.length) {
      _fail = true;
      return null;
    }
    _positions.add(_pos);
    final res = parse(this);
    final prevPos = _positions.removeLast();
    if (res == null) {
      _pos = prevPos;
      _fail = false;
    }
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
        int pos = _s.indexOf(charSpace, _pos);
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

  // takes the passed char
  bool? char(int c) => _run(() {
        if (_s[_pos] == c) {
          _pos++;
          return true;
        }
      });

  // takes space
  bool? space() => _run(() {
        if (_s[_pos] == charSpace) {
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
          n += s[i] - char0;
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
          final ok = _pos < _s.length && _s[_pos] == charEqual;
          if (ok) _pos++;
          return ok;
        }

        final pos = _pos;
        int c;
        do {
          c = _s[_pos];
        } while ((isAlphaNum(c) || c == charPlus || c == charSlash) &&
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
}

bool isDigit(int c) => c >= char0 && c <= char9;

bool isAlphaNum(int c) =>
    (c >= char0 && c <= char9) ||
    (c >= charLowerA && c <= charLowerZ) ||
    (c >= charUpperA && c <= charUpperZ);
