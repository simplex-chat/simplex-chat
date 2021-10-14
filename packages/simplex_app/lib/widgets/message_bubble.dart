import 'package:flutter/material.dart';
import 'package:simplex_chat/constants.dart';

class MessageBubble extends StatefulWidget {
  const MessageBubble({
    Key? key,
    required this.sender,
    required this.text,
    required this.isUser,
  }) : super(key: key);
  final String sender;
  final String text;
  final bool isUser;

  @override
  _MessageBubbleState createState() => _MessageBubbleState();
}

class _MessageBubbleState extends State<MessageBubble> {
  @override
  Widget build(BuildContext context) {
    return Padding(
      padding: const EdgeInsets.all(10),
      child: Column(
        crossAxisAlignment:
            widget.isUser ? CrossAxisAlignment.end : CrossAxisAlignment.start,
        children: <Widget>[
          Text(
            widget.sender,
            style: const TextStyle(fontSize: 12, color: Colors.grey),
          ),
          const SizedBox(height: 2.0),
          Material(
            borderRadius: widget.isUser
                ? const BorderRadius.only(
                    topLeft: Radius.circular(10),
                    bottomLeft: Radius.circular(10),
                    bottomRight: Radius.circular(10))
                : const BorderRadius.only(
                    topRight: Radius.circular(10),
                    bottomLeft: Radius.circular(10),
                    bottomRight: Radius.circular(10)),
            elevation: 1.0,
            color: widget.isUser ? Colors.teal : kPrimaryColor,
            child: Padding(
              padding: const EdgeInsets.symmetric(vertical: 10, horizontal: 15),
              child: Text(
                widget.text,
                style: const TextStyle(fontSize: 15, color: Colors.white),
              ),
            ),
          ),
        ],
      ),
    );
  }
}
