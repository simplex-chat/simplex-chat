import 'dart:io';

import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/views/home/home_view.dart';
import 'package:simplex_chat/widgets/custom_text_field.dart';

class SetupProfileView extends StatefulWidget {
  const SetupProfileView({Key? key}) : super(key: key);

  @override
  _SetupProfileViewState createState() => _SetupProfileViewState();
}

class _SetupProfileViewState extends State<SetupProfileView> {
  final _formKey = GlobalKey<FormState>();
  // controllers
  final TextEditingController _displayNameController = TextEditingController();
  final TextEditingController _fullNameController = TextEditingController();

  // Image Picker --> DP properties
  final imgPicker = ImagePicker();
  File? image;
  String photoUrl = '';
  bool _uploading = false;
  bool _imageUploaded = false;

  // image buttons options
  final _dpBtnText = ['Gallery', 'Camera'];
  final _dpBtnColors = [Colors.purple, Colors.green];
  final _dpBtnIcons = [Icons.photo_rounded, Icons.camera_alt_rounded];

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
                        if (value!.isEmpty) {
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
      floatingActionButton: Visibility(
        visible: MediaQuery.of(context).viewInsets.bottom == 0,
        child: FloatingActionButton(
          heroTag: 'setup',
          onPressed: () async {
            if (_formKey.currentState!.validate()) {
              FocusScope.of(context).unfocus();

              await _createProfile();

              await Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (_) => HomeView(
                    maxSlide: MediaQuery.of(context).size.width * 0.82,
                  ),
                ),
              );

              _displayNameController.clear();
              _fullNameController.clear();
              image = null;
            }
          },
          child: const Icon(Icons.check),
        ),
      ),
    );
  }

  // create profile and store in local
  Future<void> _createProfile() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    await prefs.setString('displayName', _displayNameController.text.trim());
    await prefs.setString('fullName', _fullNameController.text.trim());
    await prefs.setString(
        'photo${_displayNameController.text.trim()}', photoUrl);

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
              2,
              (index) => Column(
                mainAxisSize: MainAxisSize.min,
                children: [
                  MaterialButton(
                    color: _dpBtnColors.map((e) => e).elementAt(index),
                    shape: const CircleBorder(),
                    onPressed:
                        index == 0 ? () => _galleryPic() : () => _cameraPic(),
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
          photoUrl = file.path;
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
          _imageUploaded = true;
          photoUrl = file.path;
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
