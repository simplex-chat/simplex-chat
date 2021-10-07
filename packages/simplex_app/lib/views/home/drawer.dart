import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';

class MyDrawer extends StatelessWidget {
  const MyDrawer({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
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
                leading: const Icon(Icons.contact_phone),
                title: const Text('Your contacts'),
                subtitle: const Text('Start a conversation right away!'),
                onTap: () {},
              ),
              ListTile(
                leading: const Icon(Icons.insert_invitation),
                title: const Text('Invitations'),
                subtitle: const Text('Increase your contact circle!'),
                onTap: () {},
              ),
              ListTile(
                leading: const Icon(Icons.group),
                title: const Text('Your groups'),
                subtitle: const Text('Get in touch with numbers!'),
                onTap: () {},
              ),
              Spacer(),
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
