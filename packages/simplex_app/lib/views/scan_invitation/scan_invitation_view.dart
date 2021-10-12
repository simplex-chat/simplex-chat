import 'package:flutter/material.dart';
import 'package:share/share.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/widgets/custom_btn.dart';

class ScanInvitationView extends StatelessWidget {
  const ScanInvitationView({Key key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        title: const Text('Share QR Code'),
      ),
      body: Padding(
        padding: const EdgeInsets.all(8.0),
        child: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Text(
                'Display this QR Code for your contact to scan.',
                style: kMediumHeadingStyle,
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 25.0),
              GestureDetector(
                onTap: () => _showConnection(context),
                child: Image.asset(
                  'assets/code.png',
                ),
              ),
              const SizedBox(height: 25.0),
              const Text(
                'If you cannot share your QR Code, send the invitation via a trusted method.',
                style: kMediumHeadingStyle,
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 30.0),
              CustomButton(
                width: 200.0,
                height: 45.0,
                onPressed: _shareLink,
                color: kPrimaryColor,
                child: Row(
                  mainAxisSize: MainAxisSize.min,
                  children: const [
                    Icon(Icons.share, color: Colors.white),
                    SizedBox(width: 8.0),
                    Text(
                      'Share',
                      style: TextStyle(color: Colors.white),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  void _showConnection(BuildContext context) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            const CircleAvatar(
              backgroundImage: AssetImage('assets/dp.png'),
              radius: 70,
            ),
            const SizedBox(height: 30.0),
            const Text(
              'Bob wants to connect with you!',
              style: kMediumHeadingStyle,
              textAlign: TextAlign.center,
            ),
            const SizedBox(height: 30.0),
            CustomButton(
              width: 200,
              height: 40,
              onPressed: () => Navigator.pop(context),
              color: kPrimaryColor,
              child: const Text(
                'Confirm',
                style: TextStyle(color: Colors.white),
              ),
            ),
            const SizedBox(height: 10.0),
            CustomButton(
              width: 200,
              height: 40,
              onPressed: () => Navigator.pop(context),
              color: kSecondaryColor,
              child: const Text(
                'Ignore',
                style: TextStyle(color: Colors.white),
              ),
            ),
            const SizedBox(height: 20.0),
            const Text('Invitation was sent HH:MM')
          ],
        ),
      ),
    );
  }

  void _shareLink() {
    Share.share('Add me to SimpleX Chat via link https://someLinkHere.io');
  }
}
