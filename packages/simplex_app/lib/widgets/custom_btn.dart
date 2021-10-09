import 'package:flutter/material.dart';

class CustomButton extends StatelessWidget {
  const CustomButton(
      {@required this.width,
      @required this.height,
      @required this.onPressed,
      @required this.color,
      @required this.child,
      Key? key})
      : super(key: key);
  final double? width;
  final double? height;
  final void Function()? onPressed;
  final Widget? child;
  final Color? color;

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: width,
      height: height,
      child: MaterialButton(
        shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(5.0)),
        color: color,
        onPressed: onPressed,
        child: child,
      ),
    );
  }
}
