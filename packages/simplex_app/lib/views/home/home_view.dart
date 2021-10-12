import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:provider/provider.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';
import 'package:simplex_chat/views/contacts/conversations.dart';
import 'package:simplex_chat/views/home/drawer.dart';
import 'package:simplex_chat/views/invitations/invitation_view.dart';
import 'package:simplex_chat/views/profile/profile_view.dart';

class HomeView extends StatefulWidget {
  final double maxSlide;
  const HomeView({Key key, this.maxSlide}) : super(key: key);

  @override
  _HomeViewState createState() => _HomeViewState();
}

class _HomeViewState extends State<HomeView> {
  // views
  final List<Widget> _views = [
    const ProfileView(),
    const Conversations(),
    const Invitations(),
  ];

  @override
  Widget build(BuildContext context) {
    final _drawerProviders = Provider.of<DrawerProvider>(context);
    return WillPopScope(
      onWillPop: _onWillPop,
      child: SafeArea(
        child: Scaffold(
          drawer: const Drawer(
            child: MyDrawer(),
          ),
          body: Builder(builder: (context) {
            return Stack(
              children: [
                _views[_drawerProviders.currentIndex],
                Positioned(
                  top: MediaQuery.of(context).size.height * 0.03,
                  left: MediaQuery.of(context).size.width * 0.03,
                  child: _drawerProviders.currentIndex == 0
                      ? InkWell(
                          onTap: () {
                            _drawerProviders.currentIndex = 1;
                          },
                          child: const Padding(
                            padding: EdgeInsets.all(8.0),
                            child: Icon(Icons.arrow_back),
                          ),
                        )
                      : InkWell(
                          onTap: () {
                            Scaffold.of(context).openDrawer();
                          },
                          child: Padding(
                            padding: const EdgeInsets.all(8.0),
                            child: SvgPicture.asset('assets/menu.svg'),
                          ),
                        ),
                ),
              ],
            );
          }),
        ),
      ),
    );
  }

  Future<bool> _onWillPop() async {
    return (await showDialog(
          context: context,
          builder: (context) => AlertDialog(
            title: const Text('Exit Application', style: kMediumHeadingStyle),
            content: const Text('Are You Sure?'),
            actions: <Widget>[
              TextButton(
                child: const Text(
                  'Yes',
                  style: TextStyle(
                    color: Colors.red,
                  ),
                ),
                onPressed: () => SystemNavigator.pop(),
              ),
              TextButton(
                child: const Text('No'),
                onPressed: () => Navigator.of(context).pop(),
              ),
            ],
          ),
        )) ??
        false;
  }
}
