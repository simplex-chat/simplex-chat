import 'package:flutter/material.dart';
import 'package:qr_code_scanner/qr_code_scanner.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/widgets/custom_btn.dart';

/// Tap in the middle of QR Code to see the flow of contact being added in the Chat View

class QRCodeDetailsView extends StatelessWidget {
  final Barcode barcode;
  const QRCodeDetailsView({
    Key? key,
    required this.barcode,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('QR Code Result'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const CircleAvatar(
              backgroundImage: AssetImage('assets/dp.png'),
              radius: 90,
            ),
            const SizedBox(height: 30.0),
            Text(
              barcode.code,
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
            const SizedBox(height: 40.0),
            const Text('Invitation was sent HH:MM')
          ],
        ),
      ),
    );
  }
}
