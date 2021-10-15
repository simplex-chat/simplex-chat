import 'dart:io';

import 'package:flutter/material.dart';
import 'package:qr_code_scanner/qr_code_scanner.dart';
import 'package:simplex_chat/views/contacts/qr_code_details_view.dart';

class AddContactView extends StatefulWidget {
  const AddContactView({Key? key}) : super(key: key);

  @override
  _AddContactViewState createState() => _AddContactViewState();
}

class _AddContactViewState extends State<AddContactView> {
  bool? _dontShow = false;
  final qrKey = GlobalKey(debugLabel: 'qr');
  QRViewController? _qrViewController;
  Barcode? result;

  @override
  void reassemble() {
    super.reassemble();
    if (Platform.isAndroid) {
      _qrViewController!.pauseCamera();
    } else if (Platform.isIOS) {
      _qrViewController!.resumeCamera();
    }
  }

  // alert dialgoue
  void _initialWarning() {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text('Your profile will be sent to your contact!'),
              const SizedBox(height: 15.0),
              Row(
                children: [
                  Checkbox(
                      value: _dontShow,
                      onChanged: (value) {
                        setState(() {
                          _dontShow = value;
                        });
                      }),
                  const Text("Don't ask again")
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

  void _intiBox() {
    Future.delayed(const Duration(seconds: 1), _initialWarning);
  }

  @override
  void initState() {
    _intiBox();
    super.initState();
  }

  @override
  void dispose() {
    _qrViewController?.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Add Contact'),
        actions: [
          IconButton(
            icon: FutureBuilder(
              future: _qrViewController?.getFlashStatus(),
              builder: (context, snapshot) {
                if (snapshot.hasData) {
                  return Icon(snapshot.data == false
                      ? Icons.flash_on
                      : Icons.flash_off);
                }
                return const Icon(Icons.flash_off);
              },
            ),
            onPressed: () async {
              await _qrViewController?.toggleFlash();
              setState(() {});
            },
          ),
          IconButton(
            icon: FutureBuilder(
              future: _qrViewController?.getCameraInfo(),
              builder: (context, snapshot) {
                return const Icon(Icons.camera_alt);
              },
            ),
            onPressed: () async {
              await _qrViewController?.flipCamera();
              setState(() {});
            },
          ),
        ],
      ),
      body: SizedBox(
        height: MediaQuery.of(context).size.height,
        child: Stack(
          children: [
            Center(
              child: GestureDetector(
                  onTap: () => Navigator.of(context).pop(true),
                  child: _qrViewBuild()),
            ),
            const Center(
              child: Text(
                'Tap Here!',
                style:
                    TextStyle(color: Colors.white, fontWeight: FontWeight.bold),
              ),
            ),
            Positioned(
              top: MediaQuery.of(context).size.height * 0.15,
              left: MediaQuery.of(context).size.width * 0.125,
              child: const Text(
                'Position QR Code within the frame.',
                style: TextStyle(
                  color: Colors.white,
                  fontWeight: FontWeight.w500,
                  fontSize: 18.0,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  QRView _qrViewBuild() {
    var scanArea = (MediaQuery.of(context).size.width < 400 ||
            MediaQuery.of(context).size.height < 400)
        ? 220.0
        : 370.0;

    return QRView(
      key: qrKey,
      onQRViewCreated: (QRViewController controller) {
        _qrViewController = controller;
        controller.scannedDataStream.listen((scanData) {
          setState(() async {
            result = scanData;
            await controller.pauseCamera();
            if (result != null) {
              await Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (_) => QRCodeDetailsView(
                    barcode: result!,
                  ),
                ),
              );
              await controller.resumeCamera();
            }
          });
        });
      },
      overlay: QrScannerOverlayShape(
        borderColor: Colors.red,
        borderRadius: 0,
        borderLength: 50,
        borderWidth: 10,
        cutOutSize: scanArea,
      ),
    );
  }
}
