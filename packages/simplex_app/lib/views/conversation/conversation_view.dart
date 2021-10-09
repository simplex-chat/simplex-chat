import 'package:flutter/material.dart';
import 'package:simplex_chat/model/contact.dart';
import 'package:simplex_chat/widgets/message_bubble.dart';

class ConversationView extends StatefulWidget {
  final Contact? contact;
  const ConversationView({Key? key, @required this.contact}) : super(key: key);

  @override
  _ConversationViewState createState() => _ConversationViewState();
}

class _ConversationViewState extends State<ConversationView> {
  final ScrollController _scrollController = ScrollController();
  final TextEditingController _messageFieldController = TextEditingController();

  FocusNode? _focus;
  bool _fieldEnabled = false;
  final List<Widget> _chatMessages = [];

  @override
  void initState() {
    _focus = FocusNode();
    _focus!.addListener(() {
      debugPrint('FOCUS ${_focus!.hasFocus}');
      _fieldEnabled = _focus!.hasFocus;
      debugPrint('MESSAGE ENABLED! $_fieldEnabled');
    });
    super.initState();
  }

  @override
  void dispose() {
    _messageFieldController.dispose();
    _scrollController.dispose();
    _focus!.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () => FocusScope.of(context).unfocus(),
      child: Scaffold(
        appBar: AppBar(
          title: Text('${widget.contact!.name}'),
        ),
        body: Column(
          children: [
            Expanded(
              child: Container(
                child: _chatMessages.isEmpty
                    ? const Center(
                        child: Text('Send a message to get started!'),
                      )
                    : SingleChildScrollView(
                        controller: _scrollController,
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.stretch,
                          children: _chatMessages,
                        ),
                      ),
              ),
            ),
            Container(
              padding: const EdgeInsets.all(8.0),
              child: Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  Expanded(
                      child: SizedBox(
                    height: 45.0,
                    child: TextFormField(
                      maxLines: null,
                      focusNode: _focus,
                      textCapitalization: TextCapitalization.sentences,
                      keyboardType: TextInputType.multiline,
                      textInputAction: TextInputAction.newline,
                      controller: _messageFieldController,
                      decoration: InputDecoration(
                        contentPadding: const EdgeInsets.only(
                          left: 10.0,
                        ),
                        hintText: 'Message...',
                        hintStyle: Theme.of(context).textTheme.caption,
                        fillColor: Colors.grey[200],
                        filled: true,
                        enabledBorder: const OutlineInputBorder(
                          borderSide: BorderSide(color: Colors.transparent),
                        ),
                        focusedBorder: const OutlineInputBorder(
                          borderSide: BorderSide(color: Colors.transparent),
                        ),
                      ),
                    ),
                  )),
                  const SizedBox(width: 15.0),
                  InkWell(
                    onTap: () async {
                      if (_messageFieldController.text != "") {
                        setState(() {
                          _chatMessages.add(MessageBubble(
                            isUser: true,
                            sender: 'You',
                            text: _messageFieldController.text.trim(),
                          ));
                        });
                        _messageFieldController.clear();
                        _focus!.unfocus();
                      }
                    },
                    child: const Icon(Icons.send_rounded,
                        size: 25.0, color: Colors.teal),
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }
}
