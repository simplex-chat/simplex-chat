import 'dart:convert';

class Group {
  final String photoPath;
  final String groupName;
  final String groupDescription;
  final List<dynamic> members;

  Group(
      {this.groupName,
      this.photoPath,
      this.groupDescription,
      this.members = const []});

  factory Group.fromJson(Map<String, dynamic> json) {
    return Group(
      photoPath: json['gPhoto'],
      groupName: json['gName'],
      groupDescription: json['desc'],
      members: json['contacts'],
    );
  }

  static Map<String, dynamic> toJson(Group group) {
    return {
      'gPhoto': group.photoPath,
      'gName': group.groupName,
      'desc': group.groupDescription,
      'contacts': group.members,
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
