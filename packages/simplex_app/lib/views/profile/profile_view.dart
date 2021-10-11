import 'dart:io';

import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/widgets/custom_text_field.dart';

class ProfileView extends StatefulWidget {
  const ProfileView({Key key}) : super(key: key);

  @override
  _ProfileViewState createState() => _ProfileViewState();
}

class _ProfileViewState extends State<ProfileView> {
  final _formKey = GlobalKey<FormState>();
  // controllers
  final TextEditingController _displayNameController = TextEditingController();
  final TextEditingController _fullNameController = TextEditingController();

  // Image Picker --> DP properties
  final imgPicker = ImagePicker();
  File image;
  String _photo = '';
  bool _uploading = false;

  // image buttons options
  final _dpBtnText = ['Remove', 'Gallery', 'Camera'];
  final _dpBtnColors = [Colors.red, Colors.purple, Colors.green];
  final _dpBtnIcons = [
    Icons.delete,
    Icons.photo_rounded,
    Icons.camera_alt_rounded
  ];

  String _displayName = '';
  String _fullName = '';

  void _getUserData() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _fullName = prefs.getString('fullName');
      _displayName = prefs.getString('displayName');
      _photo = prefs.getString('photo$_displayName');
    });
    _displayNameController.text = _displayName;
    if (_fullName != null) _fullNameController.text = _fullName;
  }

  @override
  void initState() {
    _getUserData();
    super.initState();
  }

  @override
  void dispose() {
    _displayNameController.dispose();
    _fullNameController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: GestureDetector(
        onTap: () => FocusScope.of(context).unfocus(),
        child: SafeArea(
          child: SingleChildScrollView(
            child: Padding(
              padding: const EdgeInsets.all(12.0),
              child: Form(
                key: _formKey,
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.stretch,
                  children: [
                    const SizedBox(height: 30),
                    Center(
                        child: SizedBox(
                      height: 180.0,
                      width: 180.0,
                      child: Stack(
                        children: [
                          _photo != ''
                              ? CircleAvatar(
                                  radius: 100.0,
                                  backgroundImage: FileImage(File(_photo)),
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
                    )),
                    const SizedBox(height: 25.0),
                    const Text('Display Name', style: kSmallHeadingStyle),
                    const SizedBox(height: 10.0),
                    CustomTextField(
                      textEditingController: _displayNameController,
                      textInputType: TextInputType.name,
                      hintText: 'e.g John',
                      validatorFtn: (value) {
                        if (value.isEmpty) {
                          return 'Display name cannot be empty';
                        }
                        return null;
                      },
                    ),
                    const SizedBox(height: 25.0),
                    const Text('Full Name', style: kSmallHeadingStyle),
                    const SizedBox(height: 10.0),
                    CustomTextField(
                      textEditingController: _fullNameController,
                      textInputType: TextInputType.name,
                      hintText: 'e.g John Doe',
                    ),
                    const SizedBox(height: 25.0),
                    const Text(
                      'Your display name is what your contact will know you :)',
                      style: TextStyle(letterSpacing: 1.2),
                    )
                  ],
                ),
              ),
            ),
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        heroTag: 'save',
        onPressed: () async {
          if (_formKey.currentState.validate()) {
            FocusManager.instance.primaryFocus.unfocus();
            await _createProfile();
            const snackBar = SnackBar(
              backgroundColor: Colors.green,
              content: Text('Profile updated!'),
            );

            ScaffoldMessenger.of(context)
              ..hideCurrentSnackBar()
              ..showSnackBar(snackBar);
          }
        },
        child: const Icon(Icons.check),
      ),
    );
  }

  // create profile and store in local
  Future<void> _createProfile() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    await prefs.setString('displayName', _displayNameController.text.trim());
    await prefs.setString('fullName', _fullNameController.text.trim());
    await prefs.setString('photo${_displayNameController.text.trim()}', _photo);

    debugPrint(prefs.getString('photo'));
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
      image = null;
      _photo = '';
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
          _photo = file.path;
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
      debugPrint('gallery pic');
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
          _photo = file.path;
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
