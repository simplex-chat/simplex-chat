import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
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
              SvgPicture.asset(
                'assets/logo.svg',
                height: 50.0,
              ),
              const Divider(height: 50.0),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 0
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.person,
                  color: _drawerProviders.currentIndex == 0
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Your Profile',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 0
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 0;
                  Navigator.pop(context);
                },
              ),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 1
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.contact_phone,
                  color: _drawerProviders.currentIndex == 1
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Your contacts',
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
                tileColor: _drawerProviders.currentIndex == 3
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.group,
                  color: _drawerProviders.currentIndex == 3
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Your groups',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 3
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 3;
                  Navigator.pop(context);
                },
              ),
              const Spacer(),
              ListTile(
                leading: const Icon(Icons.refresh),
                title: const Text('Switch Profile'),
                subtitle: const Text('*Not supported yet!*'),
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

    await Navigator.pushNamedAndRemoveUntil(
      context,
      AppRoutes.setupProfile,
      (route) => route.settings.name == AppRoutes.setupProfile ? true : false,
    );
    String _name = prefs.getString('displayName');
    await prefs.remove('displayName');
    await prefs.remove('fullName');
    await prefs.remove('photo$_name');
  }
}
