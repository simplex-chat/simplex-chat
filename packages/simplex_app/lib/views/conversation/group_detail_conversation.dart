import 'dart:io';

import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/contact.dart';
import 'package:simplex_chat/model/group.dart';

enum MemberSetting { owner, admin, member }

class GroupDetailsConversation extends StatefulWidget {
  final Group group;
  const GroupDetailsConversation({Key? key, required this.group})
      : super(key: key);

  @override
  _GroupDetailsConversationState createState() =>
      _GroupDetailsConversationState();
}

class _GroupDetailsConversationState extends State<GroupDetailsConversation> {
  MemberSetting _memberSetting = MemberSetting.member;

  bool _addMember = false;
  List<Contact> _contactsList = []; // for storing contacts
  List _newMembers = [];

  List<Group> _groupList = [];
  List _members = [];

  // getting all members of group
  void _getMembers() {
    setState(() {
      _members = List.from(widget.group.members!);
    });
  }

  // getting groups
  void _getGroups() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String? _groups = prefs.getString('groups');
    if (_groups != null) {
      setState(() {
        _groupList = List.from(Group.decode(_groups));
      });
    }
  }

  // getting data from local storage FOR NOW
  void _getContacts() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String? _contacts = prefs.getString('contacts');
    setState(() {
      _contactsList = List.from(Contact.decode(_contacts!));
    });
  }

  @override
  void initState() {
    _getContacts();
    _getGroups();
    _getMembers();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        centerTitle: true,
        title: Text(widget.group.name!),
      ),
      body: SingleChildScrollView(
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 20.0, vertical: 12.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Center(
                child: CircleAvatar(
                  radius: 70,
                  backgroundImage: widget.group.photo == ''
                      ? const AssetImage('assets/dp.png') as ImageProvider
                      : FileImage(File(widget.group.photo!)),
                ),
              ),
              const SizedBox(height: 25.0),
              const Text('Group Name', style: kMediumHeadingStyle),
              Text(widget.group.name!),
              const Divider(),
              ListTile(
                leading: const Icon(Icons.person_add),
                title: const Text('Add a member'),
                onTap: () {
                  setState(() {
                    _addMember = !_addMember;
                  });
                },
              ),
              const Divider(),
              _newMembers.isNotEmpty
                  ? const Text('New Members Added')
                  : Container(),
              SizedBox(height: _newMembers.isNotEmpty ? 10.0 : 0.0),
              _newMembers.isNotEmpty
                  ? SingleChildScrollView(
                      scrollDirection: Axis.horizontal,
                      child: Row(
                          children: List.generate(
                        _newMembers.length,
                        (index) => Padding(
                          padding: const EdgeInsets.symmetric(horizontal: 5.0),
                          child: InkWell(
                            onTap: () {
                              setState(() {
                                _newMembers.remove(_newMembers[index]);
                              });
                            },
                            child: Container(
                              padding: const EdgeInsets.all(7.0),
                              decoration: BoxDecoration(
                                  borderRadius: BorderRadius.circular(10.0),
                                  color: Colors.grey.withAlpha(100)),
                              child: Text(_newMembers[index]),
                            ),
                          ),
                        ),
                      )),
                    )
                  : Container(),
              SizedBox(height: _addMember ? 10.0 : 0.0),
              _addMember ? const Text('Contacts Available') : Container(),
              SizedBox(height: _addMember ? 10.0 : 0.0),
              _addMember
                  ? ListView(
                      physics: const NeverScrollableScrollPhysics(),
                      shrinkWrap: true,
                      children: List.generate(
                        _contactsList.length,
                        (index) => ListTile(
                          leading: const CircleAvatar(
                            backgroundImage: AssetImage('assets/dp.png'),
                          ),
                          title: Text(_contactsList[index].name!),
                          onTap: () {
                            setState(() {
                              _newMembers.add(_contactsList[index].name);
                            });
                          },
                        ),
                      ),
                    )
                  : Container(),
              SizedBox(height: _addMember ? 10.0 : 0.0),
              const Text('Members', style: kMediumHeadingStyle),
              const SizedBox(height: 5.0),
              for (int i = 0; i < _members.length; i++)
                ListTile(
                  leading: const CircleAvatar(
                    backgroundImage: AssetImage('assets/dp.png'),
                  ),
                  title: Text(_members[i]),
                  trailing: InkWell(
                    onTap: () => _memberSettings(_members[i]),
                    child: const Padding(
                      padding: EdgeInsets.all(8.0),
                      child: Icon(Icons.settings),
                    ),
                  ),
                )
            ],
          ),
        ),
      ),
      floatingActionButton: _newMembers.isEmpty
          ? Container()
          : FloatingActionButton(
              onPressed: _addNewMembers,
              child: const Icon(Icons.check),
            ),
    );
  }

  void _memberSettings(String contact) {
    Size _size = MediaQuery.of(context).size;
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  SizedBox(width: _size.width * 0.15),
                  const Expanded(child: Text('Owner')),
                  Radio(
                      value: MemberSetting.owner,
                      groupValue: _memberSetting,
                      onChanged: (MemberSetting? value) {
                        setState(() {
                          _memberSetting = value!;
                        });
                      }),
                  SizedBox(width: _size.width * 0.15),
                ],
              ),
              Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  SizedBox(width: _size.width * 0.15),
                  const Expanded(child: Text('Admin')),
                  Radio(
                      groupValue: _memberSetting,
                      value: MemberSetting.admin,
                      onChanged: (MemberSetting? value) {
                        setState(() {
                          _memberSetting = value!;
                        });
                      }),
                  SizedBox(width: _size.width * 0.15),
                ],
              ),
              Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  SizedBox(width: _size.width * 0.15),
                  const Expanded(child: Text('Member')),
                  Radio(
                      groupValue: _memberSetting,
                      value: MemberSetting.member,
                      onChanged: (MemberSetting? value) {
                        setState(() {
                          _memberSetting = value!;
                        });
                      }),
                  SizedBox(width: _size.width * 0.15),
                ],
              ),
              const Divider(),
              TextButton(
                onPressed: () => _removeMember(contact),
                child: const Text('Remove from group',
                    style: TextStyle(color: Colors.red)),
              ),
            ],
          ),
        ),
      ),
    );
  }

  void _addNewMembers() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    // add new members to the _members list
    setState(() {
      _members = _members + _newMembers;
      _newMembers = [];
      _addMember = false;
    });

    const snackBar = SnackBar(
        backgroundColor: Colors.green, content: Text('New members added!'));
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);

    // get index of group in local
    int index = 0;

    index = _groupList.indexWhere((group) => group.name == widget.group.name);

    // add the full _members list to the group
    Group _updatedGroup = Group(
      name: widget.group.name,
      subtitle: widget.group.subtitle,
      members: _members,
      photo: widget.group.photo,
    );

    // put it in updated group local list
    _groupList[index] = _updatedGroup;

    // convert to string
    final String _updatedGroupString = Group.encode(_groupList);

    // update the group in local storage
    await prefs.setString('groups', _updatedGroupString);
  }

  void _removeMember(String contact) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();

    Navigator.pop(context);

    // remove the current member
    setState(() {
      _members.remove(contact);
    });

    var snackBar = SnackBar(
        backgroundColor: Colors.red, content: Text('$contact removed!'));
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);

    // get index of group in local
    int index = 0;

    index = _groupList.indexWhere((group) => group.name == widget.group.name);

    // new instance of group (updated)
    Group _updatedGroup = Group(
      name: widget.group.name,
      subtitle: widget.group.subtitle,
      photo: widget.group.photo,
      members: _members,
    );

    // put it in group local list
    _groupList[index] = _updatedGroup;

    // convert to string
    final String _updatedGroupString = Group.encode(_groupList);

    // update the group in local storage
    await prefs.setString('groups', _updatedGroupString);
  }
}
