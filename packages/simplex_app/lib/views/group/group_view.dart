import 'dart:io';

import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/animations/bottom_animation.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/group.dart';
import 'package:simplex_chat/views/conversation/conversation_view.dart';

class GroupView extends StatefulWidget {
  const GroupView({Key key}) : super(key: key);

  @override
  State<GroupView> createState() => _GroupViewState();
}

class _GroupViewState extends State<GroupView> {
  bool _eraseMedia = false;
  final List<String> _options = [
    'Add group',
    'Scan invitation',
  ];

  List<Group> _groupList = [];

  // delete a group
  void _deleteContact(Group group) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _groupList.remove(group);
    });
    await prefs.setString('groups', Group.encode(_groupList));
    var snackBar = SnackBar(
      backgroundColor: Colors.red,
      content: Text('${group.groupName} deleted!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }

  // getting data from local storage FOR NOW!!
  void _getGroups() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String _groups = prefs.getString('groups');
    if (_groups != null) {
      setState(() {
        _groupList = List.from(Group.decode(_groups));
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
    _getGroups();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
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
                CircleAvatar(
                  backgroundImage: _photo.isEmpty
                      ? const AssetImage('assets/dp.png') as ImageProvider
                      : FileImage(File(_photo)),
                ),
              ],
            ),
            const SizedBox(height: 15.0),
            Row(
              children: const [
                Icon(Icons.group, color: kPrimaryColor),
                SizedBox(width: 8.0),
                Text(
                  'Groups',
                  style: kHeadingStyle,
                )
              ],
            ),
            const SizedBox(height: 5.0),
            _groupList.isEmpty
                ? SizedBox(
                    height: MediaQuery.of(context).size.height * 0.7,
                    child: Center(
                      child: Column(
                        mainAxisAlignment: MainAxisAlignment.center,
                        children: const [
                          Text(
                            "You don't have any groups yet!",
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
                      _groupList.length,
                      (index) => WidgetAnimator(
                        child: ListTile(
                          leading: CircleAvatar(
                            backgroundImage: _groupList[index].photoPath == ''
                                ? const AssetImage('assets/dp.png')
                                    as ImageProvider
                                : FileImage(File(_groupList[index].photoPath)),
                          ),
                          title: Text(_groupList[index].groupName),
                          subtitle: Text(_groupList[index].groupDescription),
                          trailing: Text(
                            'Members: ${_groupList[index].members.length}',
                            style: const TextStyle(
                                fontSize: 11, color: Colors.grey),
                          ),
                          onTap: () => Navigator.push(
                            context,
                            MaterialPageRoute(
                              builder: (_) => ConversationView(
                                  name: _groupList[index].groupName),
                            ),
                          ),
                          onLongPress: () =>
                              _conversationOptions(_groupList[index]),
                        ),
                      ),
                    ),
                  ),
          ],
        ),
      ),
      floatingActionButton: PopupMenuButton(
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(5.0),
        ),
        offset: const Offset(-10, -120),
        onSelected: (value) async {
          if (value == _options[0]) {
            var value = await Navigator.pushNamed(context, AppRoutes.addGroup);
            if (value == true) {
              _getGroups();
            }
          } else {
            await Navigator.pushNamed(context, AppRoutes.scanInvitation);
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
          heroTag: 'group',
          onPressed: null,
          child: Icon(
            Icons.group_add,
          ),
        ),
      ),
    );
  }

  void _conversationOptions(Group group) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _deleteConversation(group);
                },
                child: const Text(
                  'Delete Group',
                  style: TextStyle(color: Colors.red),
                )),
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _disconnect();
                },
                child: const Text('Leave Group')),
          ],
        ),
      ),
    );
  }

  void _deleteConversation(Group group) {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text('All group history will be deleted from your device!'),
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
                _deleteContact(group);
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
                  'Leaving a group will erase all the data from your device and you will no longer be able to contact again!'),
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
  // ignore: unused_element
  void _addNewGroups() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    List<Group> _localList = [];
    final String _local = prefs.getString('groups');
    if (_local != null) {
      _localList = List.from(Group.decode(_local));
    }

    List<Group> _groups = [
      Group(
        groupName: 'Family group',
        groupDescription: 'Some description here',
        members: <String>[
          'Hamza',
          'Alice',
          'John',
          'Bob',
        ],
      ),
      Group(
        groupName: 'Friends',
        groupDescription: 'Miss you all',
        members: <String>[
          'Alice',
          'John',
          'Bob',
        ],
      ),
    ];
    _groups = _localList + _groups;

    // dummy ftn for filling the list
    final String _newGroups = Group.encode(_groups);

    await prefs.setString('groups', _newGroups);

    _getGroups();

    const snackBar = SnackBar(
      backgroundColor: Colors.green,
      content: Text('New Groups loaded!'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }
}
