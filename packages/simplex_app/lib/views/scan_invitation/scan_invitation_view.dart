import 'package:flutter/material.dart';
import 'package:share/share.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/widgets/custom_btn.dart';

class ScanInvitationView extends StatelessWidget {
  const ScanInvitationView({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    Size _size = MediaQuery.of(context).size;
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
              SizedBox(height: _size.height * 0.04),
              GestureDetector(
                onTap: () => _showConnection(context),
                child: Image.asset(
                  'assets/code.png',
                  height: _size.height * 0.3
                ),
              ),
              SizedBox(height: _size.height * 0.04),
              const Text(
                'If you cannot share your QR Code, send the invitation via a trusted method.',
                style: kMediumHeadingStyle,
                textAlign: TextAlign.center,
              ),
              SizedBox(height: _size.height * 0.04),
              CustomButton(
                width: _size.width * 0.5,
                height: _size.height * 0.055,
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
    Size _size = MediaQuery.of(context).size;
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            CircleAvatar(
              backgroundImage: const AssetImage('assets/dp.png'),
              radius: _size.height * 0.085,
            ),
            SizedBox(height: _size.height * 0.045),
            const Text(
              'Bob wants to connect with you!',
              style: kMediumHeadingStyle,
              textAlign: TextAlign.center,
            ),
            SizedBox(height: _size.height * 0.045),
            CustomButton(
              width: _size.width * 0.5,
              height: _size.height * 0.055,
              onPressed: () {
                // work around for now
                Navigator.pop(context);
                Navigator.of(context).pop(true);
              },
              color: kPrimaryColor,
              child: const Text(
                'Confirm',
                style: TextStyle(color: Colors.white),
              ),
            ),
            SizedBox(height: _size.height * 0.013),
            CustomButton(
              width: _size.width * 0.5,
              height: _size.height * 0.055,
              onPressed: () {
                int _count = 0;
                Navigator.popUntil(context, (route) => _count++ >= 2);
              },
              color: kSecondaryColor,
              child: const Text(
                'Ignore',
                style: TextStyle(color: Colors.white),
              ),
            ),
          ],
        ),
      ),
    );
  }

  void _shareLink() {
    Share.share('Add me to SimpleX Chat via link https://someLinkHere.io');
  }
}
