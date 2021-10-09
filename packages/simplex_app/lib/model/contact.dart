import 'dart:convert';

class Contact {
  final String? name;
  final String? msg;
  final String? msgTime;

  Contact({this.name, this.msg, this.msgTime});

  factory Contact.fromJson(Map<String, dynamic> json) {
    return Contact(
        name: json['name'], msg: json['msg'], msgTime: json['msgTime']);
  }

  static Map<String, dynamic> toJson(Contact contact) {
    return {
      'name': contact.name,
      'msg': contact.msg,
      'msgTime': contact.msgTime,
    };
  }

  static String encode(List<Contact> contacts) => json.encode(
        contacts
            .map<Map<String, dynamic>>((contact) => Contact.toJson(contact))
            .toList(),
      );

  static List<Contact> decode(String? contacts) =>
      (json.decode(contacts!) as List<dynamic>)
          .map<Contact>((item) => Contact.fromJson(item))
          .toList();
}
