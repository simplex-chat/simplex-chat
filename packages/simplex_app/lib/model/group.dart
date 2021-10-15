import 'dart:convert';

class Group {
  final String? name;
  final String? subtitle;
  final String? photo;
  final List<dynamic>? members;
  final bool? isGroup;

  Group({
    required this.name,
    required this.subtitle,
    this.photo = '',
    this.isGroup = true,
    this.members = const [],
  });

  factory Group.fromJson(Map<String, dynamic> json) {
    return Group(
      name: json['name'],
      subtitle: json['subtitle'],
      photo: json['photo'],
      isGroup: json['isGroup'],
      members: json['members'],
    );
  }

  static Map<String, dynamic> toJson(Group group) {
    return {
      'name': group.name,
      'subtitle': group.subtitle,
      'photo': group.photo,
      'isGroup': true,
      'members': group.members,
    };
  }

  static String encode(List<Group> groups) => json.encode(
        groups
            .map<Map<String, dynamic>>((group) => Group.toJson(group))
            .toList(),
      );

  static List<Group> decode(String groups) =>
      (json.decode(groups) as List<dynamic>)
          .map<Group>((item) => Group.fromJson(item))
          .toList();
}
