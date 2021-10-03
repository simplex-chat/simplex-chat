import "dart:typed_data";

Uint8List encodeAscii(String s) => Uint8List.fromList(s.codeUnits);

String decodeAscii(Uint8List b) => String.fromCharCodes(b);

Uint8List concat(Uint8List b1, Uint8List b2) {
  final a = Uint8List(b1.length + b2.length);
  a.setAll(0, b1);
  a.setAll(b1.length, b2);
  return a;
}

T fold<T, E>(List<E> xs, T Function(T, E) combine, T initial) {
  var res = initial;
  for (final x in xs) {
    res = combine(res, x);
  }
  return res;
}

Uint8List concatN(List<Uint8List> bs) {
  final aLen = fold(bs, (int size, Uint8List b) => size + b.length, 0);
  final a = Uint8List(aLen);
  fold(bs, (int offset, Uint8List b) {
    a.setAll(offset, b);
    return offset + b.length;
  }, 0);
  return a;
}

final char_space = " ".codeUnitAt(0);
final char_equal = "=".codeUnitAt(0);
final empty = Uint8List(0);

Uint8List unwords(Uint8List b1, Uint8List b2) {
  final a = Uint8List(b1.length + b2.length + 1);
  a.setAll(0, b1);
  a[b1.length] = char_space;
  a.setAll(b1.length + 1, b2);
  return a;
}

Uint8List unwordsN(List<Uint8List> bs) {
  var i = bs.length;
  var size = bs.length - 1;
  while (i > 0) {
    size += bs[--i].length;
  }
  final a = Uint8List(size);

  var offset = 0;
  for (i = 0; i < bs.length - 1; i++) {
    final b = bs[i];
    a.setAll(offset, b);
    offset += b.length;
    a[offset++] = char_space;
  }
  a.setAll(offset, bs[i]);
  return a;
}

final _base64chars = Uint8List.fromList(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        .codeUnits);

List<int?> __base64lookup() {
  final a = List<int?>.filled(256, null);
  for (var i = 0; i < _base64chars.length; i++) {
    a[_base64chars[i]] = i;
  }
  return a;
}

final _base64lookup = __base64lookup();

Uint8List encode64(Uint8List a) {
  final len = a.length;
  final b64len = (len / 3).ceil() * 4;
  final b64 = Uint8List(b64len);

  var j = 0;
  for (var i = 0; i < len; i += 3) {
    final e1 = i + 1 < len ? a[i + 1] : 0;
    final e2 = i + 2 < len ? a[i + 2] : 0;
    b64[j++] = _base64chars[a[i] >> 2];
    b64[j++] = _base64chars[((a[i] & 3) << 4) | (e1 >> 4)];
    b64[j++] = _base64chars[((e1 & 15) << 2) | (e2 >> 6)];
    b64[j++] = _base64chars[e2 & 63];
  }

  if (len % 3 != 0) b64[b64len - 1] = char_equal;
  if (len % 3 == 1) b64[b64len - 2] = char_equal;

  return b64;
}

Uint8List? decode64(Uint8List b64) {
  var len = b64.length;
  if (len % 4 != 0) return null;
  var bLen = (len * 3) >> 2;

  if (b64[len - 1] == char_equal) {
    len--;
    bLen--;
    if (b64[len - 1] == char_equal) {
      len--;
      bLen--;
    }
  }

  final bytes = Uint8List(bLen);

  var i = 0;
  var pos = 0;
  while (i < len) {
    final enc1 = _base64lookup[b64[i++]];
    final enc2 = i < len ? _base64lookup[b64[i++]] : 0;
    final enc3 = i < len ? _base64lookup[b64[i++]] : 0;
    final enc4 = i < len ? _base64lookup[b64[i++]] : 0;
    if (enc1 == null || enc2 == null || enc3 == null || enc4 == null) {
      return null;
    }
    bytes[pos++] = (enc1 << 2) | (enc2 >> 4);
    var p = pos++;
    if (p < bLen) bytes[p] = ((enc2 & 15) << 4) | (enc3 >> 2);
    p = pos++;
    if (p < bLen) bytes[p] = ((enc3 & 3) << 6) | (enc4 & 63);
  }

  return bytes;
}
