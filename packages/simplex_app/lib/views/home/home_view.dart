import 'dart:io';

import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';
import 'package:simplex_chat/views/contacts/conversations.dart';
import 'package:simplex_chat/views/home/drawer.dart';
import 'package:simplex_chat/views/invitations/invitation_view.dart';
import 'package:simplex_chat/views/profile/profile_view.dart';

class HomeView extends StatefulWidget {
  final double maxSlide;
  const HomeView({Key? key, required this.maxSlide}) : super(key: key);

  @override
  _HomeViewState createState() => _HomeViewState();
}

class _HomeViewState extends State<HomeView> {
  String? _photo = '';
  String? _displayName = '';

  // views
  final List<Widget> _views = [
    const ProfileView(),
    const Conversations(),
    const Invitations(),
  ];

  void _getUserData() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    setState(() {
      _displayName = prefs.getString('displayName');
      _photo = prefs.getString('photo$_displayName');
    });
  }

  @override
  void initState() {
    _getUserData();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    final _drawerProviders = Provider.of<DrawerProvider>(context);
    return WillPopScope(
      onWillPop: _onWillPop,
      child: Scaffold(
        drawer: const Drawer(
          child: MyDrawer(),
        ),
        body: SafeArea(
          child: Builder(builder: (context) {
            return Stack(
              children: [
                _views[_drawerProviders.currentIndex],
                _drawerProviders.currentIndex == 0
                    ? Positioned(
                        top: MediaQuery.of(context).size.height * 0.017,
                        left: MediaQuery.of(context).size.width * 0.02,
                        child: InkWell(
                          onTap: () {
                            _drawerProviders.currentIndex = 1;
                            _getUserData();
                          },
                          child: const Padding(
                            padding: EdgeInsets.all(8.0),
                            child: Icon(Icons.arrow_back, color: kPrimaryColor),
                          ),
                        ),
                      )
                    : _drawerProviders.currentIndex == 2
                        ? Padding(
                            padding: const EdgeInsets.all(10.0),
                            child: Row(
                              mainAxisSize: MainAxisSize.min,
                              children: [
                                InkWell(
                                  onTap: () {
                                    _drawerProviders.currentIndex = 1;
                                  },
                                  child: const Padding(
                                      padding: EdgeInsets.all(8.0),
                                      child: Icon(Icons.arrow_back,
                                          color: kPrimaryColor)),
                                ),
                                const Spacer(),
                                InkWell(
                                    onTap: () {},
                                    child: const Padding(
                                      padding: EdgeInsets.all(8.0),
                                      child: Icon(
                                        Icons.bug_report,
                                        color: Colors.grey,
                                      ),
                                    )),
                                const SizedBox(width: 10.0),
                                Column(
                                  mainAxisSize: MainAxisSize.min,
                                  crossAxisAlignment: CrossAxisAlignment.end,
                                  children: [
                                    Text('Hi! $_displayName',
                                        style: kSmallHeadingStyle),
                                    const Text('Good day!'),
                                  ],
                                ),
                                const SizedBox(width: 10.0),
                                GestureDetector(
                                  onTap: () {
                                    _drawerProviders.currentIndex = 0;
                                  },
                                  child: CircleAvatar(
                                    backgroundImage: _photo!.isEmpty
                                        ? const AssetImage('assets/dp.png')
                                            as ImageProvider
                                        : FileImage(File(_photo!)),
                                  ),
                                )
                              ],
                            ),
                          )
                        : Padding(
                            padding: const EdgeInsets.all(10.0),
                            child: Row(
                              mainAxisSize: MainAxisSize.min,
                              children: [
                                InkWell(
                                  onTap: () {
                                    Scaffold.of(context).openDrawer();
                                  },
                                  child: Padding(
                                    padding: const EdgeInsets.all(8.0),
                                    child: SvgPicture.asset('assets/menu.svg'),
                                  ),
                                ),
                                const Spacer(),
                                Column(
                                  mainAxisSize: MainAxisSize.min,
                                  crossAxisAlignment: CrossAxisAlignment.end,
                                  children: [
                                    Text('Hi! $_displayName',
                                        style: kSmallHeadingStyle),
                                    const Text('Good day!'),
                                  ],
                                ),
                                const SizedBox(width: 10.0),
                                GestureDetector(
                                  onTap: () {
                                    _drawerProviders.currentIndex = 0;
                                  },
                                  child: CircleAvatar(
                                    backgroundImage: _photo!.isEmpty
                                        ? const AssetImage('assets/dp.png')
                                            as ImageProvider
                                        : FileImage(File(_photo!)),
                                  ),
                                )
                              ],
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
