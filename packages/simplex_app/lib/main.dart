import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/custom_scroll_behavior.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';
import 'package:simplex_chat/views/contacts/add_contact_view.dart';
import 'package:simplex_chat/views/contacts/add_contact_web.dart';
import 'package:simplex_chat/views/group/add_group_view.dart';
import 'package:simplex_chat/views/home/home_view.dart';
import 'package:simplex_chat/views/onboarding/intro_view.dart';
import 'package:simplex_chat/views/scan_invitation/scan_invitation_view.dart';
import 'package:simplex_chat/views/setup_profile_view.dart';
import 'package:simplex_chat/views/splash_screen.dart';
import 'package:url_strategy/url_strategy.dart';

/// Basic Structure is setup on [Providers]
/// Navigations are [namedRoutes]

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  setPathUrlStrategy();
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    final theme = ThemeData(
      primarySwatch: Colors.teal,
      primaryColor: kPrimaryColor,
    );
    return MultiProvider(
      providers: [
        ChangeNotifierProvider(create: (_) => DrawerProvider()),
      ],
      child: Consumer<DrawerProvider>(
        builder: (context, drawerProvider, chidl) => MaterialApp(
          debugShowCheckedModeBanner: false,
          title: 'SimpleX Chat',
          theme: theme.copyWith(
            colorScheme: theme.colorScheme.copyWith(secondary: kPrimaryColor),
            primaryColorLight: Colors.white,
          ),
          builder: (context, widget) {
            return ScrollConfiguration(
                behavior: const ScrollBehaviorModified(), child: widget!);
          },
          initialRoute: AppRoutes.splash,
          routes: <String, WidgetBuilder>{
            AppRoutes.splash: (_) => const SplashScreen(),
            AppRoutes.intro: (_) => const IntroView(),
            AppRoutes.setupProfile: (_) => const SetupProfileView(),
            AppRoutes.home: (_) => const HomeView(),
            AppRoutes.addContact: (_) => const AddContactView(),
            AppRoutes.addContactWeb: (_) => const AddContactWeb(),
            AppRoutes.scanInvitation: (_) => const ScanInvitationView(),
            AppRoutes.addGroup: (_) => const AddGroupView(),
          },
        ),
      ),
    );
  }
}
