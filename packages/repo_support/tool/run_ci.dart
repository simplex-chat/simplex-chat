// ignore_for_file: prefer_double_quotes

import 'package:dev_test/package.dart';
import 'package:path/path.dart';

Future main() async {
  for (var dir in [
    'simplex_app',
    'simplexmq',
    'repo_support',
  ]) {
    await packageRunCi(join('..', dir));
  }
}
