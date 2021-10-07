import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:simplex_chat/animations/bottomAnimation.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';

class HomeViewWidget extends StatefulWidget {
  HomeViewWidget({Key? key}) : super(key: key);

  @override
  _HomeViewWidgetState createState() => _HomeViewWidgetState();
}

class _HomeViewWidgetState extends State<HomeViewWidget> {
  bool _haveConnections = false;
  bool? _eraseMedia = false;

  final List<String> _options = [
    'Add contact',
    'Scan invitation',
    'New group',
  ];

  final List<String> _userNames = [
    'Bob',
    'John',
    'Alice',
    'Arya',
    'Maria',
  ];

  final List<String> _lastMsg = [
    'Okay!',
    'Hey there!',
    'Will get back to you.',
    'Hi',
    'Okay got it',
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: SingleChildScrollView(
        child: Padding(
          padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
          child: Center(
            child: Column(
              children: [
                Align(
                  alignment: Alignment.centerRight,
                  child: GestureDetector(
                    onTap: () {
                      setState(() {
                        _haveConnections = !_haveConnections;
                      });
                    },
                    child: SvgPicture.asset(
                      'assets/logo.svg',
                      height: 40.0,
                    ),
                  ),
                ),
                const SizedBox(height: 10.0),
                !_haveConnections
                    ? Container(
                        height: MediaQuery.of(context).size.height * 0.7,
                        child: Center(
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.center,
                            children: [
                              const Text(
                                "You don't have any conversation yet!",
                                style: kMediumHeadingStyle,
                                textAlign: TextAlign.center,
                              ),
                              const SizedBox(height: 8.0),
                              const Text(
                                "Click the icon below to add a contact",
                                textAlign: TextAlign.center,
                              ),
                            ],
                          ),
                        ),
                      )
                    : ListView(
                        shrinkWrap: true,
                        children: List.generate(
                          _userNames.length,
                          (index) => WidgetAnimator(
                            child: ListTile(
                              leading: CircleAvatar(
                                backgroundImage: AssetImage('assets/dp.png'),
                              ),
                              title: Text(_userNames[index]),
                              subtitle: Text(_lastMsg[index]),
                              trailing: const Text(
                                'Just now',
                                style:
                                    TextStyle(color: Colors.grey, fontSize: 12),
                              ),
                              onTap: () {},
                              onLongPress: _conversationOptions,
                            ),
                          ),
                        ),
                      ),
              ],
            ),
          ),
        ),
      ),
      floatingActionButton: PopupMenuButton(
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(5.0),
        ),
        offset: Offset(-10, -180),
        onSelected: (value) {
          if (value == _options[0]) {
            Navigator.pushNamed(context, AppRoutes.addContact);
          } else if (value == _options[1]) {
            Navigator.pushNamed(context, AppRoutes.scanInvitation);
          } else {
            Navigator.pushNamed(context, AppRoutes.addGroup);
          }
        },
        itemBuilder: (context) => _options
            .map(
              (opt) => PopupMenuItem(
                value: opt,
                child: Text(opt),
              ),
            )
            .toList(),
        child: FloatingActionButton(
          heroTag: 'connect',
          onPressed: null,
          child: const Icon(
            Icons.person_add,
          ),
        ),
      ),
    );
  }

  void _conversationOptions() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _deleteConversation();
                },
                child: const Text(
                  'Delete Conversation',
                  style: TextStyle(color: Colors.red),
                )),
            TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _disconnect();
                },
                child: const Text('Disconnect')),
          ],
        ),
      ),
    );
  }

  void _deleteConversation() {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text(
                  'All conversation history will be deleted from your device!'),
              const SizedBox(height: 15.0),
              Row(
                children: [
                  Checkbox(
                      value: _eraseMedia,
                      onChanged: (value) {
                        setState(() {
                          _eraseMedia = value;
                        });
                      }),
                  const Text('Erase files & Media')
                ],
              ),
            ],
          ),
          actions: [
            InkWell(
              onTap: () => Navigator.pop(context),
              child: Padding(
                padding: const EdgeInsets.all(8.0),
                child: const Icon(Icons.check, color: Colors.green),
              ),
            ),
            InkWell(
              onTap: () => Navigator.pop(context),
              child: Padding(
                padding: const EdgeInsets.all(8.0),
                child: const Icon(Icons.cancel_outlined, color: Colors.red),
              ),
            )
          ],
        ),
      ),
    );
  }

  void _disconnect() {
    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) => AlertDialog(
          title: const Text('Are you Sure?'),
          content: Column(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Text(
                  'Disconnecting will erase all the data from your device and you will no longer be able to contact again!'),
              const SizedBox(height: 15.0),
              Row(
                children: [
                  Checkbox(
                      value: _eraseMedia,
                      onChanged: (value) {
                        setState(() {
                          _eraseMedia = value;
                        });
                      }),
                  const Text('Erase files & Media')
                ],
              ),
            ],
          ),
          actions: [
            InkWell(
              onTap: () => Navigator.pop(context),
              child: Padding(
                padding: const EdgeInsets.all(8.0),
                child: const Icon(Icons.check, color: Colors.green),
              ),
            ),
            InkWell(
              onTap: () => Navigator.pop(context),
              child: Padding(
                padding: const EdgeInsets.all(8.0),
                child: const Icon(Icons.cancel_outlined, color: Colors.red),
              ),
            )
          ],
        ),
      ),
    );
  }
}
