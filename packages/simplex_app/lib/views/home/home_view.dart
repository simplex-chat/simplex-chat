import 'dart:math' as math;
import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:provider/provider.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';
import 'package:simplex_chat/views/group/group_view.dart';

import 'package:simplex_chat/views/home/drawer.dart';
import 'package:simplex_chat/views/home/home_view_widget.dart';
import 'package:simplex_chat/views/invitations/invitation_view.dart';

class HomeView extends StatefulWidget {
  final double? maxSlide;
  const HomeView({
    Key? key,
    this.maxSlide,
  }) : super(key: key);

  @override
  _HomeViewState createState() => _HomeViewState();
}

class _HomeViewState extends State<HomeView> with TickerProviderStateMixin {
  AnimationController? animationController;
  bool? _canBeDragged;

  // views
  final List<Widget> _views = [
    const HomeViewWidget(),
    const Invitations(),
    const GroupView(),
  ];

  @override
  void initState() {
    super.initState();
    animationController = AnimationController(
        vsync: this, duration: const Duration(milliseconds: 250));
  }

  @override
  Widget build(BuildContext context) {
    final _drawerProviders = Provider.of<DrawerProvider>(context);
    return GestureDetector(
      onHorizontalDragStart: _onDragStart,
      onHorizontalDragUpdate: _onDragUpdate,
      onHorizontalDragEnd: _onDragEnd,
      behavior: HitTestBehavior.translucent,
      child: AnimatedBuilder(
        animation: animationController!,
        builder: (context, _) {
          return Material(
            color: Colors.white70,
            child: SafeArea(
              child: Stack(
                children: [
                  Transform.translate(
                    offset: Offset(
                        widget.maxSlide! * (animationController!.value - 1), 0),
                    child: Transform(
                      transform: Matrix4.identity()
                        ..setEntry(3, 2, 0.001)
                        ..rotateY(
                            math.pi / 2 * (1 - animationController!.value)),
                      alignment: Alignment.centerRight,
                      child: MyDrawer(
                        animationController: animationController,
                      ),
                    ),
                  ),
                  Transform.translate(
                    offset: Offset(
                        widget.maxSlide! * animationController!.value, 0),
                    child: Transform(
                        transform: Matrix4.identity()
                          ..setEntry(3, 2, 0.001)
                          ..rotateY(-math.pi / 2 * animationController!.value),
                        alignment: Alignment.centerLeft,
                        child: _views[_drawerProviders.currentIndex]),
                  ),
                  Positioned(
                    top: MediaQuery.of(context).padding.top,
                    left: MediaQuery.of(context).size.width * 0.03 +
                        animationController!.value * widget.maxSlide!,
                    child: InkWell(
                      onTap: () {
                        _drawerProviders.toggle(animationController);
                      },
                      child: Padding(
                        padding: const EdgeInsets.all(8.0),
                        child: SvgPicture.asset(
                          'assets/menu.svg',
                        ),
                      ),
                    ),
                  ),
                ],
              ),
            ),
          );
        },
      ),
    );
  }

  void _onDragStart(DragStartDetails details) {
    bool isDragOpenFromLeft = animationController!.isDismissed;
    bool isDragCloseFromRight = animationController!.isCompleted;
    _canBeDragged = isDragOpenFromLeft || isDragCloseFromRight;
  }

  void _onDragUpdate(DragUpdateDetails details) {
    if (_canBeDragged!) {
      double delta = details.primaryDelta! / widget.maxSlide!;
      animationController!.value += delta;
    }
  }

  void _onDragEnd(DragEndDetails details) {
    double _kMinFlingVelocity = 365.0;

    if (animationController!.isDismissed || animationController!.isCompleted) {
      return;
    }
    if (details.velocity.pixelsPerSecond.dx.abs() >= _kMinFlingVelocity) {
      double visualVelocity = details.velocity.pixelsPerSecond.dx /
          MediaQuery.of(context).size.width;

      animationController!.fling(velocity: visualVelocity);
    } else if (animationController!.value < 0.5) {
      animationController!.reverse();
    } else {
      animationController!.forward();
    }
  }
}
