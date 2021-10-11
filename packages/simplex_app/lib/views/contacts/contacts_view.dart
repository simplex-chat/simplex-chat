import 'dart:io';

import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/animations/bottom_animation.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/contact.dart';
import 'package:simplex_chat/views/conversation/conversation_view.dart';

class ContactsView extends StatefulWidget {
  const ContactsView({Key key}) : super(key: key);

  @override
  _ContactsViewState createState() => _ContactsViewState();
}

class _ContactsViewState extends State<ContactsView> {
  bool _eraseMedia = false;

  List<Contact> _contactsList = []; // for storing contacts

  final List<String> _options = [
    'Add contact',
    'Scan invitation',
  ];

  // delete a contact
  void _deleteContact(Contact contact) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _contactsList.remove(contact);
    });
    await prefs.setString('contacts', Contact.encode(_contactsList));
    var snackBar = SnackBar(
      backgroundColor: Colors.red,
      content: Text('${contact.name} deleted!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }

  // getting data from local storage FOR NOW!!
  void _getContacts() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String _contacts = prefs.getString('contacts');
    if (_contacts != null) {
      setState(() {
        _contactsList = List.from(Contact.decode(_contacts));
      });
    }
  }

  String _photo = '';
  String _displayName = '';

  void _getUserData() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _displayName = prefs.getString('displayName');
      _photo = prefs.getString('photo$_displayName');
    });
  }

  @override
  void initState() {
    _getUserData();
    _getContacts();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: SingleChildScrollView(
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
          child: Center(
            child: Column(
              children: [
                Row(
                  mainAxisAlignment: MainAxisAlignment.end,
                  children: [
                    Column(
                      crossAxisAlignment: CrossAxisAlignment.end,
                      children: [
                        Text('Hi! $_displayName', style: kSmallHeadingStyle),
                        const Text('Good day!'),
                      ],
                    ),
                    const SizedBox(width: 10.0),
                    GestureDetector(
                      onTap: _addNewContacts,
                      child: CircleAvatar(
                        backgroundImage: _photo.isEmpty
                            ? const AssetImage('assets/dp.png') as ImageProvider
                            : FileImage(File(_photo)),
                      ),
                    )
                  ],
                ),
                const SizedBox(height: 15.0),
                Row(
                  children: const [
                    Icon(Icons.chat, color: kPrimaryColor),
                    SizedBox(width: 8.0),
                    Text(
                      'Chats',
                      style: kHeadingStyle,
                    )
                  ],
                ),
                const SizedBox(height: 5.0),
                _contactsList.isEmpty
                    ? SizedBox(
                        height: MediaQuery.of(context).size.height * 0.7,
                        child: Center(
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.center,
                            children: const [
                              Text(
                                "You don't have any conversation yet!",
                                style: kMediumHeadingStyle,
                                textAlign: TextAlign.center,
                              ),
                              SizedBox(height: 8.0),
                              Text(
                                'Click the icon below to add a contact',
                                textAlign: TextAlign.center,
                              ),
                            ],
                          ),
                        ),
                      )
                    : ListView(
                        shrinkWrap: true,
                        children: List.generate(
                          _contactsList.length,
                          (index) => WidgetAnimator(
                            child: ListTile(
                              leading: const CircleAvatar(
                                backgroundImage: AssetImage('assets/dp.png'),
                              ),
                              title: Text(_contactsList[index].name),
                              subtitle: Text(_contactsList[index].msg),
                              trailing: Text(
                                _contactsList[index].msgTime,
                                style: const TextStyle(
                                    fontSize: 11, color: Colors.grey),
                              ),
                              onTap: () => Navigator.push(
                                context,
                                MaterialPageRoute(
                                  builder: (_) => ConversationView(
                                    name: _contactsList[index].name,
                                  ),
                                ),
                              ),
                              onLongPress: () =>
                                  _conversationOptions(_contactsList[index]),
                            ),
                          ),
                        ),
                      ),
              ],
            ),
          ),
        ),
      ),
      floatingActionButton: PopupMenuButton(
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(5.0),
        ),
        offset: const Offset(-10, -120),
        onSelected: (value) {
          if (value == _options[0]) {
            Navigator.pushNamed(context, AppRoutes.scanInvitation);
          } else {
            Navigator.pushNamed(context, AppRoutes.addContact);
          }
        },
        itemBuilder: (context) => _options
            .map(
              (opt) => PopupMenuItem(
                value: opt,
                child: Text(opt),
              ),
            )
            .toList(),
        child: const FloatingActionButton(
          heroTag: 'connect',
          onPressed: null,
          child: Icon(
            Icons.person_add,
          ),
        ),
      ),
    );
  }

  void _conversationOptions(Contact contact) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _deleteConversation(contact);
                },
                child: const Text(
                  'Delete Conversation',
                  style: TextStyle(color: Colors.red),
                )),
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _disconnect();
                },
                child: const Text('Disconnect')),
          ],
        ),
      ),
    );
  }

  void _deleteConversation(Contact contact) {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text(
                  'All conversation history will be deleted from your device!'),
              const SizedBox(height: 15.0),
              Row(
                children: [
                  Checkbox(
                      value: _eraseMedia,
                      onChanged: (value) {
                        setState(() {
                          _eraseMedia = value;
                        });
                      }),
                  const Text('Erase files & Media')
                ],
              ),
            ],
          ),
          actions: [
            InkWell(
              onTap: () {
                _deleteContact(contact);
                Navigator.pop(context);
              },
              child: const Padding(
                padding: EdgeInsets.all(8.0),
                child: Icon(Icons.check, color: Colors.green),
              ),
            ),
            InkWell(
              onTap: () => Navigator.pop(context),
              child: const Padding(
                padding: EdgeInsets.all(8.0),
                child: Icon(Icons.cancel_outlined, color: Colors.red),
              ),
            )
          ],
        ),
      ),
    );
  }

  void _disconnect() {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text(
                  'Disconnecting will erase all the data from your device and you will no longer be able to contact again!'),
              const SizedBox(height: 15.0),
              Row(
                children: [
                  Checkbox(
                      value: _eraseMedia,
                      onChanged: (value) {
                        setState(() {
                          _eraseMedia = value;
                        });
                      }),
                  const Text('Erase files & Media')
                ],
              ),
            ],
          ),
          actions: [
            InkWell(
              onTap: () => Navigator.pop(context),
              child: const Padding(
                padding: EdgeInsets.all(8.0),
                child: Icon(Icons.check, color: Colors.green),
              ),
            ),
            InkWell(
              onTap: () => Navigator.pop(context),
              child: const Padding(
                padding: EdgeInsets.all(8.0),
                child: Icon(Icons.cancel_outlined, color: Colors.red),
              ),
            )
          ],
        ),
      ),
    );
  }

  // dummy ftn for loading new contacts
  void _addNewContacts() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();

    List<Contact> _localList = [];
    final String _local = prefs.getString('contacts');
    if (_local != null) {
      _localList = List.from(Contact.decode(_local));
    }

    List<Contact> _newList = [
      Contact(
        name: 'Harry',
        msg: 'Hello!',
        msgTime: 'Just now',
      ),
      Contact(
        name: 'Ayesha',
        msg: 'OK!',
        msgTime: 'Just now',
      ),
      Contact(
        name: 'Larry',
        msg: 'Yep, Already done!',
        msgTime: 'Just now',
      ),
    ];
    _newList = _localList + _newList;

    // dummy ftn for filling the list
    final String _newContacts = Contact.encode(_newList);

    await prefs.setString('contacts', _newContacts);
    _getContacts();
    const snackBar = SnackBar(
      backgroundColor: Colors.green,
      content: Text('New contacts loaded!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }
}
