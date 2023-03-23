| 31.01.2023 | FR, [EN](/docs/SQL.md), [CZ](/docs/lang/cs/SQL.md) |

# Accès aux messages de la base de données

## Déchiffrer les bases de données

Afin de visualiser les données de la base de données, vous devez d'abord les déchiffrer. Installez `sqlcipher` en utilisant votre gestionnaire de paquets préféré et exécutez les commandes suivantes dans le répertoire contenant les bases de données :
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# S'assurer qu'il fonctionne bien
select * from users;
```

Si vous voyez `Parse error : no such table : users`, assurez-vous que vous avez entré la bonne phrase secrète, et que vous avez changé la phrase secrète au hasard dans l'application Android (si vous avez obtenu cette base de données à partir d'un appareil Android, bien sûr).

# Requêtes SQL

Vous pouvez exécuter des requêtes `direct_messages`, `group_messages` et `all_messages` (ou leurs alternatives plus simples `direct_messages_plain`, `group_messages_plain` et `all_messages_plain`), par exemple :

```sql
-- vous pouvez mettre ces paramètres ou ceux que vous préférez dans ~/.sqliterc
-- pour maintenir les sessions du client sqlite3
.mode column
.headers on
.nullvalue NULL

-- vues simples pour direct, group et all_messages
-- avec les messages de l'utilisateur dédupliqués pour group et all_messages ;
-- seuls les événements de chat "x.msg.new" ("nouveau message") - filtre les événements de service ;
-- msg_sent est 0 pour reçu, 1 pour envoyé
select * from direct_messages_plain;
select * from group_messages_plain;
select * from all_messages_plain;

-- demander d'autres détails de votre historique de chat avec le SQL régulier, par exemple :
-- les fichiers que vous avez soumis pour l'envoi
select * from direct_messages where msg_sent = 1 and chat_msg_event = 'x.file';
-- tout ce que Catherine a envoyé lié aux chats
select * from direct_messages where msg_sent = 0 and contact = 'catherine' and msg_body like '%cats%';
-- toute correspondance avec alice dans #team
select * from group_messages where group_name = 'team' and contact = 'alice';

-- regrouper vos données de chat
select contact_or_group, num_messages from (
  select
    contact as contact_or_group, count(1) as num_messages
    from direct_messages_plain group by contact
  union
  select
    group_name as contact_or_group, count(1) as num_messages
    from group_messages_plain group by group_name
)
order by num_messages desc;
```
