import 'dart:io';

import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/model/contact.dart';
import 'package:simplex_chat/model/group.dart';
import 'package:simplex_chat/widgets/custom_text_field.dart';

class AddGroupView extends StatefulWidget {
  const AddGroupView({Key? key}) : super(key: key);

  @override
  _AddGroupViewState createState() => _AddGroupViewState();
}

class _AddGroupViewState extends State<AddGroupView> {
  // Image Picker --> DP properties
  final imgPicker = ImagePicker();
  File? image;
  String _groupPhotoPath = '';
  bool _uploading = false;
  bool _imageUploaded = false;

  final List _members = [];
  final _formKey = GlobalKey<FormState>();
  final _displayNameController = TextEditingController();
  final _descController = TextEditingController();

  bool _addMember = false;
  List<Contact> _contactsList = []; // for storing contacts

  // image buttons options
  final _dpBtnText = ['Remove', 'Gallery', 'Camera'];
  final _dpBtnColors = [Colors.red, Colors.purple, Colors.green];
  final _dpBtnIcons = [
    Icons.delete,
    Icons.photo_rounded,
    Icons.camera_alt_rounded
  ];

  // getting data from local storage FOR NOW
  void _getContacts() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    final String? _contacts = prefs.getString('contacts');
    setState(() {
      _contactsList = List.from(Contact.decode(_contacts!));
    });
  }

  String _userPhotoPath = '';
  void _getUserPhoto() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    String? _photo = prefs.getString('photo${prefs.getString('displayName')}');
    if (_photo != null) {
      setState(() {
        _userPhotoPath = _photo;
      });
    }
    debugPrint(_userPhotoPath);
  }

  @override
  void initState() {
    _getUserPhoto();
    _getContacts();
    super.initState();
  }

  @override
  void dispose() {
    _displayNameController.dispose();
    _descController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () => FocusScope.of(context).unfocus(),
      child: Scaffold(
        appBar: AppBar(
          leading: BackButton(
            onPressed: () => Navigator.of(context).pop(false),
          ),
          title: const Text('New Group'),
        ),
        body: SingleChildScrollView(
          child: Padding(
            padding: const EdgeInsets.all(12.0),
            child: Form(
              key: _formKey,
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  const SizedBox(height: 10.0),
                  Center(
                    child: SizedBox(
                      height: 180.0,
                      width: 180.0,
                      child: Stack(
                        children: [
                          _imageUploaded
                              ? CircleAvatar(
                                  radius: 100.0,
                                  backgroundImage:
                                      FileImage(File(_groupPhotoPath)),
                                )
                              : const CircleAvatar(
                                  radius: 100.0,
                                  backgroundImage: AssetImage('assets/dp.png'),
                                ),
                          Positioned(
                            right: 0,
                            bottom: 0,
                            child: FloatingActionButton(
                              backgroundColor: kSecondaryColor,
                              elevation: 2.0,
                              mini: true,
                              onPressed: _updateProfilePic,
                              child: _uploading
                                  ? const SizedBox(
                                      height: 18.0,
                                      width: 18.0,
                                      child: CircularProgressIndicator(
                                        strokeWidth: 2.0,
                                        valueColor:
                                            AlwaysStoppedAnimation<Color>(
                                                Colors.white),
                                      ),
                                    )
                                  : const Icon(
                                      Icons.add_a_photo,
                                      size: 20,
                                    ),
                            ),
                          )
                        ],
                      ),
                    ),
                  ),
                  const SizedBox(height: 25.0),
                  const Text('Group Name', style: kSmallHeadingStyle),
                  const SizedBox(height: 10.0),
                  CustomTextField(
                    textEditingController: _displayNameController,
                    textInputType: TextInputType.name,
                    hintText: 'e.g College friends',
                    validatorFtn: (String? value) {
                      if (value!.isEmpty) {
                        return 'Group name cannot be empty';
                      }
                      return null;
                    },
                  ),
                  const SizedBox(height: 10.0),
                  const Text('Group Description', style: kSmallHeadingStyle),
                  const SizedBox(height: 10.0),
                  CustomTextField(
                    textEditingController: _descController,
                    textInputType: TextInputType.text,
                    hintText: 'e.g Friends from UK',
                  ),
                  const SizedBox(height: 10.0),
                  _members.isNotEmpty
                      ? const Text('Members Added')
                      : Container(),
                  SizedBox(height: _members.isNotEmpty ? 10.0 : 0.0),
                  _members.isNotEmpty
                      ? SingleChildScrollView(
                          scrollDirection: Axis.horizontal,
                          child: Row(
                              children: List.generate(
                            _members.length,
                            (index) => Padding(
                              padding:
                                  const EdgeInsets.symmetric(horizontal: 5.0),
                              child: InkWell(
                                onTap: () {
                                  setState(() {
                                    _members.remove(_members[index]);
                                  });
                                },
                                child: Container(
                                  padding: const EdgeInsets.all(7.0),
                                  decoration: BoxDecoration(
                                      borderRadius: BorderRadius.circular(10.0),
                                      color: Colors.grey.withAlpha(100)),
                                  child: Text(_members[index]),
                                ),
                              ),
                            ),
                          )),
                        )
                      : Container(),
                  SizedBox(height: _members.isNotEmpty ? 10.0 : 0.0),
                  ListTile(
                    leading: const Icon(Icons.person_add),
                    title: const Text('Add a member'),
                    onTap: () {
                      setState(() {
                        _addMember = !_addMember;
                      });
                    },
                  ),
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
                                  _members.add(_contactsList[index].name);
                                });
                              },
                            ),
                          ),
                        )
                      : Container(),
                  const Divider(height: 30.0),
                  ListTile(
                      leading: CircleAvatar(
                        backgroundImage: _userPhotoPath == ''
                            ? const AssetImage('assets/dp.png') as ImageProvider
                            : FileImage(File(_userPhotoPath)),
                      ),
                      title: const Text('You'),
                      subtitle: const Text(
                        'Owner',
                        style: TextStyle(color: Colors.grey, fontSize: 12.0),
                      )),
                ],
              ),
            ),
          ),
        ),
        floatingActionButton: Visibility(
          visible: MediaQuery.of(context).viewInsets.bottom == 0,
          child: FloatingActionButton(
            heroTag: 'setup',
            onPressed: () async {
              if (_formKey.currentState!.validate()) {
                FocusScope.of(context).unfocus();
                _addNewGroup(_displayNameController.text.trim(),
                    _descController.text.trim());
                _descController.clear();
                _displayNameController.clear();
                Navigator.of(context).pop(true);
              }
            },
            child: const Icon(Icons.check),
          ),
        ),
      ),
    );
  }

  void _updateProfilePic() {
    showModalBottomSheet(
      shape: const RoundedRectangleBorder(
        borderRadius: BorderRadius.only(
          topLeft: Radius.circular(10.0),
          topRight: Radius.circular(10.0),
        ),
      ),
      context: context,
      builder: (context) => Padding(
        padding: const EdgeInsets.symmetric(horizontal: 12.0, vertical: 20.0),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Container(
              decoration: BoxDecoration(
                  color: Colors.grey,
                  borderRadius: BorderRadius.circular(360.0)),
              height: 7.0,
              width: 50.0,
            ),
            const SizedBox(height: 20.0),
            const Align(
              alignment: Alignment.centerLeft,
              child: Text(
                ' Profile photo',
                style: kHeadingStyle,
              ),
            ),
            const SizedBox(height: 15.0),
            Row(
                children: List.generate(
              3,
              (index) => Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  MaterialButton(
                    color: _dpBtnColors.map((e) => e).elementAt(index),
                    shape: const CircleBorder(),
                    onPressed: index == 0
                        ? () => _removePic()
                        : index == 1
                            ? () => _galleryPic()
                            : () => _cameraPic(),
                    child: Icon(
                      _dpBtnIcons.map((e) => e).elementAt(index),
                      color: Colors.white,
                    ),
                  ),
                  Text(
                    _dpBtnText.map((e) => e).elementAt(index),
                    textAlign: TextAlign.center,
                  )
                ],
              ),
            ))
          ],
        ),
      ),
    );
  }

  void _removePic() {
    setState(() {
      _imageUploaded = false;
      image = null;
      _groupPhotoPath = '';
    });
    Navigator.pop(context);
  }

  void _cameraPic() async {
    try {
      setState(() {
        _uploading = true;
      });

      // picking Image from Camera
      final file = await imgPicker.getImage(
        source: ImageSource.camera,
      );

      if (file != null) {
        image = File(file.path);
        setState(() {
          _uploading = false;
          _imageUploaded = true;
          _groupPhotoPath = file.path;
        });
      } else {
        setState(() {
          _uploading = false;
        });
      }

      Navigator.pop(context);
    } catch (e) {
      rethrow;
    }
  }

  void _galleryPic() async {
    try {
      setState(() {
        _uploading = true;
      });

      // picking Image from local storage
      final file = await imgPicker.getImage(
        source: ImageSource.gallery,
      );

      if (file != null) {
        image = File(file.path);
        setState(() {
          _uploading = false;
          _imageUploaded = true;
          _groupPhotoPath = file.path;
        });
      } else {
        setState(() {
          _uploading = false;
        });
      }

      Navigator.pop(context);
    } catch (e) {
      rethrow;
    }
  }

  void _addNewGroup(String name, String desc) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    List<Group> _localList = [];
    final String? _local = prefs.getString('groups');
    if (_local != null) {
      _localList = List.from(Group.decode(_local));
    }
    List<Group> _groups = [
      Group(
        name: name,
        photo: _groupPhotoPath,
        subtitle: desc,
        members: _members,
      ),
    ];
    debugPrint(_groups[0].isGroup.toString());
    _groups = _localList + _groups;

    final String _newGroups = Group.encode(_groups);

    await prefs.setString('groups', _newGroups);

    var snackBar = SnackBar(
      backgroundColor: Colors.green,
      content: Text('$name added'),
    );
    ScaffoldMessenger.of(context)
      ..hideCurrentSnackBar()
      ..showSnackBar(snackBar);
  }
}
