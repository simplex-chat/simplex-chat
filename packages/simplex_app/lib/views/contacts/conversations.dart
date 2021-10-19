import 'dart:io';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/animations/bottom_animation.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/contact.dart';
import 'package:simplex_chat/model/group.dart';
import 'package:simplex_chat/views/conversation/conversation_view.dart';

class Conversations extends StatefulWidget {
  const Conversations({Key? key}) : super(key: key);

  @override
  _ConversationsState createState() => _ConversationsState();
}

class _ConversationsState extends State<Conversations> {
  bool? _eraseMedia = false;

  List<dynamic> _conversations = [];

  List<Contact> _contactsList = []; // for storing contacts
  List<Group> _groupList = []; // for storing groups

  final List<String> _options = [
    'Add contact',
    'Scan invitation',
    'Add group',
  ];

  // getting data from local storage FOR NOW!!
  void _getContacts() async {
    debugPrint('Getting contacts!');
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String? _contacts = prefs.getString('contacts');
    if (_contacts != null) {
      setState(() {
        _contactsList = List.from(Contact.decode(_contacts));
      });
    }
  }

  // getting data from local storage FOR NOW!!
  void _getGroups() async {
    debugPrint('Getting groups!');
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String? _groups = prefs.getString('groups');
    if (_groups != null) {
      setState(() {
        _groupList = List.from(Group.decode(_groups));
      });
    }

    _gettingGroupContactsChats();
  }

  void _gettingGroupContactsChats() {
    debugPrint('Merging!');
    setState(() {
      _conversations = List.from(_contactsList);
      _conversations = _conversations + _groupList;
    });
  }

  @override
  void initState() {
    _getContacts();
    _getGroups();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        elevation: 0.0,
        backgroundColor: Colors.transparent,
        leadingWidth: 30,
        title: InkWell(
          onTap: _addNewContacts,
          child: const Padding(
            padding: EdgeInsets.all(8.0),
            child: Icon(
              Icons.bug_report,
              color: Colors.grey,
              size: 22.0,
            ),
          ),
        ),
      ),
      backgroundColor: Colors.white,
      body: SingleChildScrollView(
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
          child: Center(
            child: Column(
              children: [
                const SizedBox(height: 10.0),
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
                        physics: const NeverScrollableScrollPhysics(),
                        shrinkWrap: true,
                        children: List.generate(
                          _conversations.length,
                          (index) => WidgetAnimator(
                            child: ListTile(
                              leading: CircleAvatar(
                                backgroundImage:
                                    // ignore: avoid_dynamic_calls
                                    _conversations[index].photo == ''
                                        ? const AssetImage('assets/dp.png')
                                            as ImageProvider
                                        : FileImage(
                                            // ignore: avoid_dynamic_calls
                                            File(_conversations[index].photo ??
                                                '')),
                              ),
                              // ignore: avoid_dynamic_calls
                              title: Text(_conversations[index].name ?? ''),
                              subtitle:
                                  // ignore: avoid_dynamic_calls
                                  Text(_conversations[index].subtitle ?? ''),
                              // ignore: avoid_dynamic_calls
                              trailing: Icon(
                                // ignore: avoid_dynamic_calls
                                _conversations[index].isGroup
                                    ? Icons.group
                                    : Icons.person,
                                size: 18,
                                color: Colors.grey[400],
                              ),
                              onTap: () async {
                                var value = await Navigator.push(
                                  context,
                                  MaterialPageRoute(
                                    builder: (_) => ConversationView(
                                      data: _conversations[index],
                                    ),
                                  ),
                                );
                                value ??= false;
                                if (value) {
                                  _getContacts();
                                  _getGroups();
                                }
                              },
                              onLongPress: () =>
                                  _conversationOptions(_conversations[index]),
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
        offset: const Offset(-10, -155),
        onSelected: (value) async {
          if (value == _options[0]) {
            var newMember =
                await Navigator.pushNamed(context, AppRoutes.scanInvitation);
            newMember ??= false;
            if (newMember == true) {
              _addNewMember();
            }
          } else if (value == _options[1]) {
            var newMember = await Navigator.pushNamed(context,
                kIsWeb ? AppRoutes.addContactWeb : AppRoutes.addContact);
            newMember ??= false;
            if (newMember == true) {
              _addNewMember();
            }
          } else {
            var newGroup =
                await Navigator.pushNamed(context, AppRoutes.addGroup);
            if (newGroup == true) {
              _getGroups();
            }
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

  // delete a group
  void _deleteGroup(Group group) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _groupList.remove(group);
    });
    await prefs.setString('groups', Group.encode(_groupList));
    var snackBar = SnackBar(
      backgroundColor: Colors.red,
      content: Text('${group.name} deleted!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }

  void _conversationOptions(var chat) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _deleteConversation(chat);
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

  void _deleteConversation(var chat) {
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
              // ignore: avoid_dynamic_calls
              onTap: chat.isGroup
                  ? () {
                      _deleteGroup(chat);
                      _conversations.remove(chat);
                      Navigator.pop(context);
                    }
                  : () {
                      _deleteContact(chat);
                      _conversations.remove(chat);
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

    // adding dummy contact
    List<Contact> _localList = [];
    final String? _local = prefs.getString('contacts');
    if (_local != null) {
      _localList = List.from(Contact.decode(_local));
    }

    List<Contact> _newList = [
      Contact(
        name: 'Harry',
        subtitle: 'Hello!',
      ),
    ];
    _newList = _localList + _newList;

    // dummy ftn for filling the list
    final String _newContacts = Contact.encode(_newList);

    await prefs.setString('contacts', _newContacts);

    // adding dummy contact
    List<Group> _localListGroup = [];
    final String? _localGroups = prefs.getString('groups');
    if (_local != null) {
      _localListGroup = List.from(Group.decode(_localGroups!));
    }

    List<Group> _newGroups = [
      Group(
          name: 'College Friends',
          subtitle: 'Lovely people',
          members: ['Alice', 'James', 'Rio']),
    ];
    _newGroups = _localListGroup + _newGroups;

    // dummy ftn for filling the list
    final String _newGroup = Group.encode(_newGroups);

    await prefs.setString('groups', _newGroup);

    _getContacts();
    _getGroups();

    const snackBar = SnackBar(
      backgroundColor: Colors.green,
      content: Text('New contacts loaded!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }

  void _addNewMember() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    // adding dummy contact
    List<Contact> _localList = [];
    final String? _local = prefs.getString('contacts');
    if (_local != null) {
      _localList = List.from(Contact.decode(_local));
    }

    List<Contact> _newList = [
      Contact(
        name: 'Bob',
        subtitle: 'You and Bob are connected now!',
      ),
    ];
    _newList = _localList + _newList;

    // dummy ftn for filling the list
    final String _newContacts = Contact.encode(_newList);

    await prefs.setString('contacts', _newContacts);

    _getContacts();
    _getGroups();

    const snackBar = SnackBar(
      backgroundColor: Colors.green,
      content: Text('New connection added!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }
}
