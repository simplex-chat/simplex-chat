import 'package:flutter/material.dart';
import 'package:qr_code_scanner/qr_code_scanner.dart';

class AddContactWeb extends StatefulWidget {
  const AddContactWeb({Key? key}) : super(key: key);

  @override
  _AddContactWebState createState() => _AddContactWebState();
}

class _AddContactWebState extends State<AddContactWeb> {
  bool? _dontShow = false;

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
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        title: const Text('Add Contact'),
      ),
      body: SizedBox(
        height: MediaQuery.of(context).size.height,
        child: Stack(
          children: [
            Center(
              child: GestureDetector(
                onTap: () => Navigator.of(context).pop(true),
                child: Container(
                    padding: const EdgeInsets.all(5.0),
                    decoration: BoxDecoration(
                      color: Colors.white,
                      border: Border.all(
                        color: Colors.red,
                        width: 5.0,
                      ),
                    ),
                    child: Image.asset('assets/code.png')),
              ),
            ),
            Positioned(
              top: MediaQuery.of(context).size.height * 0.15,
              left: MediaQuery.of(context).size.width * 0.41,
              child: const Text(
                'Position QR Code within the frame.',
                style: TextStyle(
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
}
