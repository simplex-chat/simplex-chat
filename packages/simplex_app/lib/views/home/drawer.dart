import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:provider/provider.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';

class MyDrawer extends StatelessWidget {
  final AnimationController? animationController;
  const MyDrawer({Key? key, this.animationController}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final _drawerProviders = Provider.of<DrawerProvider>(context);
    return SizedBox(
      width: MediaQuery.of(context).size.width * 0.82,
      child: Material(
        color: Colors.white,
        child: Padding(
          padding: const EdgeInsets.all(10.0),
          child: Column(
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
                  Icons.contact_phone,
                  color: _drawerProviders.currentIndex == 0
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Your contacts',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 0
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                subtitle: Text(
                  'Start a conversation right away!',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 0
                        ? Colors.white
                        : Colors.grey,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 0;
                  _drawerProviders.toggle(animationController);
                },
              ),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 1
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.insert_invitation,
                  color: _drawerProviders.currentIndex == 1
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Invitations',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 1
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                subtitle: Text(
                  'Increase your contact circle!',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 1
                        ? Colors.white
                        : Colors.grey,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 1;
                  _drawerProviders.toggle(animationController);
                },
              ),
              ListTile(
                tileColor: _drawerProviders.currentIndex == 2
                    ? kPrimaryColor
                    : Colors.transparent,
                leading: Icon(
                  Icons.group,
                  color: _drawerProviders.currentIndex == 2
                      ? Colors.white
                      : Colors.grey,
                ),
                title: Text(
                  'Your groups',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 2
                        ? Colors.white
                        : Colors.black,
                  ),
                ),
                subtitle: Text(
                  'Get in touch with numbers!',
                  style: TextStyle(
                    color: _drawerProviders.currentIndex == 2
                        ? Colors.white
                        : Colors.grey,
                  ),
                ),
                onTap: () {
                  _drawerProviders.currentIndex = 2;
                  _drawerProviders.toggle(animationController);
                },
              ),
              const Spacer(),
              ListTile(
                leading: const Icon(Icons.exit_to_app_rounded),
                title: const Text('Logout'),
                subtitle: const Text('Good bye! See you soon :)'),
                onTap: () => Navigator.pop(context),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
