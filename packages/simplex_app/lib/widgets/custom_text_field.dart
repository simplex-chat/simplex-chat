import 'package:flutter/material.dart';

class CustomTextField extends StatefulWidget {
  final TextEditingController textEditingController;
  final TextInputType textInputType;

  final String hintText;

  final String? Function(String?)? validatorFtn;

  const CustomTextField({
    Key? key,
    required this.textEditingController,
    required this.textInputType,
    required this.hintText,
    this.validatorFtn,
  }) : super(key: key);

  @override
  _CustomTextFieldState createState() => _CustomTextFieldState();
}

class _CustomTextFieldState extends State<CustomTextField> {
  final FocusNode _node = FocusNode();

  @override
  void dispose() {
    _node.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    double width = MediaQuery.of(context).size.width;

    return Container(
      width: width * 0.89,
      decoration: BoxDecoration(borderRadius: BorderRadius.circular(8.0)),
      child: TextFormField(
        textCapitalization: TextCapitalization.sentences,
        controller: widget.textEditingController,
        textInputAction: TextInputAction.done,
        keyboardType: widget.textInputType,
        decoration: InputDecoration(
          contentPadding: const EdgeInsets.symmetric(horizontal: 15.0),
          hintText: widget.hintText,
          hintStyle: Theme.of(context).textTheme.caption,
          fillColor: Colors.grey[200],
          filled: true,
          enabledBorder: const OutlineInputBorder(
              borderSide: BorderSide(color: Colors.transparent)),
          focusedBorder: const OutlineInputBorder(
            borderSide: BorderSide(color: Colors.transparent),
          ),
          errorBorder: const OutlineInputBorder(
            borderSide: BorderSide(color: Colors.red),
          ),
          focusedErrorBorder: const OutlineInputBorder(
            borderSide: BorderSide(color: Colors.red),
          ),
        ),
        validator: widget.validatorFtn,
      ),
    );
  }
}
