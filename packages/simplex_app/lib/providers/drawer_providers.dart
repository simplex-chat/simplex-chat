import 'package:flutter/cupertino.dart';

class DrawerProvider extends ChangeNotifier {
  int _currentIndex = 0;

  int get currentIndex => _currentIndex;

  set currentIndex(int value) {
    _currentIndex = value;
    notifyListeners();
  }

  // toggle drawer
  TickerFuture toggle(
      AnimationController? animationController) {
    
    return animationController!.isDismissed
        ? animationController.forward()
        : animationController.reverse();
  }
}
