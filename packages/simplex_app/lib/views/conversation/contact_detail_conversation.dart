import 'package:flutter/material.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/contact.dart';

class ContactDetailsConversation extends StatelessWidget {
  final Contact contact;
  const ContactDetailsConversation({Key key, this.contact}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(contact.name),
      ),
      body: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 20.0, vertical: 12.0),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const CircleAvatar(
              radius: 65,
              backgroundImage: AssetImage('assets/dp.png'),
            ),
            const SizedBox(height: 15.0),
            const Text('Display Name', style: kMediumHeadingStyle),
            Text(contact.name),
          ],
        ),
      ),
    );
  }
}
