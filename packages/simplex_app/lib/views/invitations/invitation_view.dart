import 'dart:io';

import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';

class Invitations extends StatefulWidget {
  const Invitations({Key key}) : super(key: key);

  @override
  State<Invitations> createState() => _InvitationsState();
}

class _InvitationsState extends State<Invitations> {
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
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      color: Colors.white,
      child: Padding(
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
                Icon(Icons.inventory_outlined, color: kPrimaryColor),
                SizedBox(width: 8.0),
                Text(
                  'Invitations',
                  style: kHeadingStyle,
                )
              ],
            ),
            const SizedBox(height: 5.0),
            SizedBox(
              height: MediaQuery.of(context).size.height * 0.7,
              child: const Center(
                child: Text(
                  "You don't have any invitations yet!",
                  style: kMediumHeadingStyle,
                  textAlign: TextAlign.center,
                ),
              ),
            )
          ],
        ),
      ),
    );
  }
}
