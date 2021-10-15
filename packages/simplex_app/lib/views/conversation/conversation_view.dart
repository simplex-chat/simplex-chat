import 'dart:io';

import 'package:flutter/material.dart';
import 'package:simplex_chat/views/conversation/contact_detail_conversation.dart';
import 'package:simplex_chat/views/conversation/group_detail_conversation.dart';
import 'package:simplex_chat/widgets/message_bubble.dart';

class ConversationView extends StatefulWidget {
  // ignore: prefer_typing_uninitialized_variables
  final data;
  const ConversationView({Key? key, this.data}) : super(key: key);

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
      debugPrint('MESSAGE ENABLED $_fieldEnabled');
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
          leading: BackButton(
            onPressed: () => Navigator.of(context).pop(true),
          ),
          leadingWidth: MediaQuery.of(context).size.width * 0.085,
          title: InkWell(
            // ignore: avoid_dynamic_calls
            onTap: widget.data.isGroup
                ? () {
                    Navigator.push(
                      context,
                      MaterialPageRoute(
                        builder: (_) => GroupDetailsConversation(
                          group: widget.data,
                        ),
                      ),
                    );
                  }
                : () {
                    Navigator.push(
                      context,
                      MaterialPageRoute(
                        builder: (_) => ContactDetailsConversation(
                          contact: widget.data,
                        ),
                      ),
                    );
                  },
            child: Padding(
              padding: const EdgeInsets.symmetric(vertical: 8.0),
              child: Row(
                children: [
                  CircleAvatar(
                    radius: 15,
                    // ignore: avoid_dynamic_calls
                    backgroundImage: widget.data.photo == ''
                        ? const AssetImage('assets/dp.png') as ImageProvider
                        : FileImage(
                            // ignore: avoid_dynamic_calls
                            File(widget.data.photo),
                          ),
                  ),
                  const SizedBox(width: 10.0),
                  Text(
                    // ignore: avoid_dynamic_calls
                    widget.data.name,
                  ),
                ],
              ),
            ),
          ),
          actions: [
            IconButton(
              onPressed: () {
                setState(() {
                  _chatMessages.add(MessageBubble(
                    isUser: false,
                    // ignore: avoid_dynamic_calls
                    sender: widget.data.isGroup
                        // ignore: avoid_dynamic_calls
                        ? widget.data.members.length == 0
                            ? 'some person'
                            // ignore: avoid_dynamic_calls
                            : widget.data.members[0]
                        // ignore: avoid_dynamic_calls
                        : widget.data.name,
                    text: 'Hey there! How is it going?',
                  ));
                });
              },
              icon: const Icon(
                Icons.message,
              ),
            )
          ],
        ),
        body: Column(
          children: [
            Expanded(
              child: Container(
                child: _chatMessages.isEmpty
                    ? const Center(
                        child: Text('Send a message to get started'),
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
                  const SizedBox(width: 10.0),
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
                        enabledBorder: OutlineInputBorder(
                          borderRadius: BorderRadius.circular(360),
                          borderSide:
                              const BorderSide(color: Colors.transparent),
                        ),
                        focusedBorder: OutlineInputBorder(
                          borderRadius: BorderRadius.circular(360),
                          borderSide:
                              const BorderSide(color: Colors.transparent),
                        ),
                      ),
                    ),
                  )),
                  IconButton(
                    onPressed: () {
                      if (_messageFieldController.text.isNotEmpty) {
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
                    icon: const Icon(Icons.send_rounded,
                        size: 25.0, color: Colors.teal),
                  ),
                ],
              ),
            ),
            SizedBox(
              height: MediaQuery.of(context).size.height * 0.02,
            )
          ],
        ),
      ),
    );
  }
}
