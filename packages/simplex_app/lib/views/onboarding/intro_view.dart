import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import 'package:simplex_chat/app_routes.dart';
import 'package:simplex_chat/constants.dart';

class IntroView extends StatelessWidget {
  const IntroView({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(12.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              SvgPicture.asset(
                'assets/logo.svg',
                height: 80.0,
              ),
              SizedBox(height: MediaQuery.of(context).size.height * 0.05),
              const Text(
                'Complete your profile to begin using SimpleX. Your profile is local to your device and will help identify you to your connections.',
                style: kMediumHeadingStyle,
              )
            ],
          ),
        ),
      ),
      floatingActionButton: FloatingActionButton(
        heroTag: 'welcome',
        onPressed: () => Navigator.pushNamed(context, AppRoutes.setupProfile),
        child: const Icon(
          Icons.arrow_forward,
        ),
      ),
    );
  }
}
