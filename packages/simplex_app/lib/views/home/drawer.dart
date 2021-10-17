import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';

class MyDrawer extends StatelessWidget {
  const MyDrawer({Key? key}) : super(key: key);

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
              SvgPicture.asset(
                'assets/logo.svg',
                height: 55.0,
              ),
              const Divider(height: 30.0),
              ListTile(
                leading: const Icon(Icons.insert_invitation),
                title: const Text('Invitations'),
                onTap: () {
                  _drawerProviders.currentIndex = 2;
                  Navigator.pop(context);
                },
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

  // remove the locally stored user data
  void _switchProfile(BuildContext context) async {
    SharedPreferences prefs = await SharedPreferences.getInstance();

    int _count = 0;
    Navigator.of(context).popUntil((route) => _count++ >= 2);
    String? _name = prefs.getString('displayName');
    await prefs.remove('displayName');
    await prefs.remove('fullName');
    await prefs.remove('photo$_name');
  }
}
