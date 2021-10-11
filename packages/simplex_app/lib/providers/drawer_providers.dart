import 'package:flutter/cupertino.dart';

class DrawerProvider extends ChangeNotifier {
  int _currentIndex = 1;

  int get currentIndex => _currentIndex;

  set currentIndex(int value) {
    _currentIndex = value;
    notifyListeners();
  }
}
