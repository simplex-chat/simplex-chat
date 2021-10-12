import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';

class MyDrawer extends StatelessWidget {
  const MyDrawer({Key key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final _drawerProviders = Provider.of<DrawerProvider>(context);
    return SizedBox(
      width: MediaQuery.of(context).size.width * 0.82,
      child: Padding(
        padding: const EdgeInsets.all(10.0),
        child: Builder(builder: (context) {
          return Column(
            children: [
              const SizedBox(height: 30.0),
              Image.asset(
                'assets/simpleX.png',
                height: 85.0,
              ),
              const Divider(height: 50.0),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 1
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.chat,
                  color: _drawerProviders.currentIndex == 1
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Conversations',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 1
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 1;
                  Navigator.pop(context);
                },
              ),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 2
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.insert_invitation,
                  color: _drawerProviders.currentIndex == 2
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Invitations',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 2
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 2;
                  Navigator.pop(context);
                },
              ),
              ListTile(
                leading: const Icon(
                  Icons.settings,
                  color: Colors.grey,
                ),
                title: const Text('Settings'),
                onTap: () {},
              ),
              const Spacer(),
              ListTile(
                leading: const Icon(Icons.refresh),
                title: const Text('Switch Profile'),
                subtitle: const Text(
                  'Not supported yet!',
                  style: TextStyle(fontStyle: FontStyle.italic),
                ),
                onTap: () => _switchProfile(context),
              ),
            ],
          );
        }),
      ),
    );
  }

  void _switchProfile(BuildContext context) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();

    int _count = 0;
    Navigator.of(context).popUntil((route) => _count++ >= 2);
    String _name = prefs.getString('displayName');
    await prefs.remove('displayName');
    await prefs.remove('fullName');
    await prefs.remove('photo$_name');
  }
}
