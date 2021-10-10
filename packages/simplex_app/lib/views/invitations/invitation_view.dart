import 'package:flutter/material.dart';
import 'package:flutter_svg/svg.dart';
import 'package:simplex_chat/constants.dart';

class Invitations extends StatelessWidget {
  const Invitations({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return Container(
      color: Colors.white,
      child: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
        child: Column(
          children: [
            Align(
              alignment: Alignment.centerRight,
              child: SvgPicture.asset(
                'assets/logo.svg',
                height: 40.0,
              ),
            ),
            const SizedBox(height: 15.0),
            Row(
              children: const [
                Icon(Icons.inventory_outlined, color: kPrimaryColor),
                SizedBox(width: 8.0),
                Text(
                  'Invitations',
                  style: kHeadingStyle,
                )
              ],
            ),
            const SizedBox(height: 5.0),
            SizedBox(
              height: MediaQuery.of(context).size.height * 0.7,
              child: const Center(
                child: Text(
                  "You don't have any invitations yet!",
                  style: kMediumHeadingStyle,
                  textAlign: TextAlign.center,
                ),
              ),
            )
          ],
        ),
      ),
    );
  }
}
