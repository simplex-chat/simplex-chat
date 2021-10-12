import 'dart:convert';

class Contact {
  final String name;
  final String subtitle;
  final String photo;
  final bool isGroup;

  Contact({
    this.name,
    this.subtitle,
    this.photo = '',
    this.isGroup = false,
  });

  factory Contact.fromJson(Map<String, dynamic> json) {
    return Contact(
      name: json['name'],
      subtitle: json['subtitle'],
      photo: json['photo'],
      isGroup: json['isGroup'],
    );
  }

  static Map<String, dynamic> toJson(Contact contact) {
    return {
      'name': contact.name,
      'subtitle': contact.subtitle,
      'photo': contact.photo,
      'isGroup': false,
    };
  }

  static String encode(List<Contact> contacts) => json.encode(
        contacts
            .map<Map<String, dynamic>>((contact) => Contact.toJson(contact))
            .toList(),
      );

  static List<Contact> decode(String contacts) =>
      (json.decode(contacts) as List<dynamic>)
          .map<Contact>((item) => Contact.fromJson(item))
          .toList();
}
