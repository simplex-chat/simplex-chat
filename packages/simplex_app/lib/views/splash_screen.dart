import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:progress_indicators/progress_indicators.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:simplex_chat/animations/entrance_fader.dart';
import 'package:simplex_chat/constants.dart';
import 'package:simplex_chat/views/home/home_view.dart';
import 'package:simplex_chat/views/onBoarding/intro_view.dart';

class SplashScreen extends StatefulWidget {
  const SplashScreen({Key? key}) : super(key: key);

  @override
  _SplashScreenState createState() => _SplashScreenState();
}

class _SplashScreenState extends State<SplashScreen> {
  // logincheck
  void _loginCheck() async {
    SharedPreferences prefs = await SharedPreferences.getInstance();
    String? _name = prefs.getString('displayName');
    Future.delayed(const Duration(seconds: 4), () {
      Navigator.push(
        context,
        MaterialPageRoute(
          builder: (_) => _name == null
              ? const IntroView()
              : HomeView(
                  maxSlide: MediaQuery.of(context).size.width * 0.82,
                ),
        ),
      );
    });
  }

  @override
  void initState() {
    _loginCheck();
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            EntranceFader(
              duration: const Duration(seconds: 1),
              offset: const Offset(0, 20),
              child: SvgPicture.asset(
                'assets/logo.svg',
                height: 70,
              ),
            ),
            EntranceFader(
              offset: const Offset(0, 00),
              duration: const Duration(seconds: 1),
              delay: const Duration(seconds: 1),
              child: JumpingDotsProgressIndicator(
                fontSize: 40,
                color: kPrimaryColor,
              ),
            ),
          ],
        ),
      ),
    );
  }
}
