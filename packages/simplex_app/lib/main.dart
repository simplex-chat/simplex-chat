import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/custom_scroll_behavior.dart';
import 'package:simplex_chat/providers/drawer_providers.dart';
import 'package:simplex_chat/views/contacts/add_contact_view.dart';
import 'package:simplex_chat/views/group/add_group_view.dart';
import 'package:simplex_chat/views/onboarding/intro_view.dart';
import 'package:simplex_chat/views/scanInvitation/scan_invitation_view.dart';
import 'package:simplex_chat/views/setup_profile_view.dart';
import 'package:simplex_chat/views/splash_screen.dart';

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({Key key}) : super(key: key);

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
          ),
          builder: (context, widget) {
            return ScrollConfiguration(
                behavior: const ScrollBehaviorModified(), child: widget);
          },
          initialRoute: AppRoutes.splash,
          routes: <String, WidgetBuilder>{
            AppRoutes.splash: (_) => const SplashScreen(),
            AppRoutes.intro: (_) => const IntroView(),
            AppRoutes.setupProfile: (_) => const SetupProfileView(),
            AppRoutes.addContact: (_) => const AddContactView(),
            AppRoutes.scanInvitation: (_) => const ScanInvitationView(),
            AppRoutes.addGroup: (_) => const AddGroupView(),
          },
        ),
      ),
    );
  }
}
