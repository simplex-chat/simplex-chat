import 'dart:io';

import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/widgets/custom_text_field.dart';

class AddGroupView extends StatefulWidget {
  const AddGroupView({Key? key}) : super(key: key);

  @override
  _AddGroupViewState createState() => _AddGroupViewState();
}

class _AddGroupViewState extends State<AddGroupView> {
  final _displayNameController = TextEditingController();

  @override
  void dispose() {
    _displayNameController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: () => FocusScope.of(context).unfocus(),
      child: Scaffold(
        appBar: AppBar(
          title: const Text('New Group'),
        ),
        body: Padding(
          padding: const EdgeInsets.all(12.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              const SizedBox(height: 10.0),
              const Center(
                child: GroupDP(),
              ),
              const SizedBox(height: 25.0),
              const Text('Group Name', style: kSmallHeadingStyle),
              const SizedBox(height: 10.0),
              CustomTextField(
                textEditingController: _displayNameController,
                textInputType: TextInputType.name,
                hintText: 'e.g College friends',
                validatorFtn: (value) {
                  if (value!.isEmpty) {
                    return 'Group name cannot be empty!';
                  }
                  return null;
                },
              ),
              const SizedBox(height: 10.0),
              ListTile(
                leading: const Icon(Icons.person_add),
                title: const Text('Add a member'),
                onTap: () {},
              ),
              const Divider(height: 30.0),
              const ListTile(
                  leading: CircleAvatar(
                    backgroundImage: AssetImage('assets/dp.png'),
                  ),
                  title: Text('You'),
                  trailing: Text(
                    'Owner',
                    style: TextStyle(color: Colors.grey, fontSize: 12.0),
                  )),
            ],
          ),
        ),
        floatingActionButton: Visibility(
          visible: MediaQuery.of(context).viewInsets.bottom == 0,
          child: FloatingActionButton(
            heroTag: 'setup',
            onPressed: () {
              FocusScope.of(context).unfocus();
              Navigator.pop(context);
            },
            child: const Icon(Icons.check),
          ),
        ),
      ),
    );
  }
}

class GroupDP extends StatefulWidget {
  const GroupDP({Key? key}) : super(key: key);

  @override
  _GroupDPState createState() => _GroupDPState();
}

class _GroupDPState extends State<GroupDP> {
  // Image Picker --> DP properties
  final imgPicker = ImagePicker();
  File? image;
  String photoUrl = '';
  bool _uploading = false;
  bool _imageUploaded = false;

  // image buttons options
  final _dpBtnText = ['Remove', 'Gallery', 'Camera'];
  final _dpBtnColors = [Colors.red, Colors.purple, Colors.green];
  final _dpBtnIcons = [
    Icons.delete,
    Icons.photo_rounded,
    Icons.camera_alt_rounded
  ];

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      height: 180.0,
      width: 180.0,
      child: Stack(
        children: [
          _imageUploaded
              ? CircleAvatar(
                  radius: 100.0,
                  backgroundImage: FileImage(image!),
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
                        valueColor: AlwaysStoppedAnimation<Color>(Colors.white),
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
}
