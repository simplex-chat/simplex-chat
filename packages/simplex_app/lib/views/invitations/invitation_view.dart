import 'package:flutter/material.dart';
import 'package:simplex_chat/constants.dart';

class Invitations extends StatefulWidget {
  const Invitations({Key? key}) : super(key: key);

  @override
  State<Invitations> createState() => _InvitationsState();
}

class _InvitationsState extends State<Invitations> {
  @override
  void initState() {
    super.initState();
  }

  @override
  Widget build(BuildContext context) {
    return Container(
      color: Colors.white,
      child: Padding(
        padding: const EdgeInsets.symmetric(horizontal: 15.0, vertical: 20.0),
        child: Column(
          children: [
            const SizedBox(height: 40.0),
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
                  style: kSmallHeadingStyle,
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
