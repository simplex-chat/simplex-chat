| Zaktualizowano 31.01.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SQL.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/SQL.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/SQL.md) |

# Dostęp do wiadomości w bazie danych

## Odszyfrowywanie baz danych

Aby zobaczyć dane z bazy danych musisz je najpierw odszyfrować. Zainstaluj `sqlcipher` za pomocą swojego ulubionego menedżera pakietów i uruchom następujące polecenia w katalogu z bazami danych:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Upewnij się, że działa dobrze
select * from users;
```

Jeśli zobaczysz `Parse error: no such table: users`, upewnij się, że wpisałeś poprawne hasło i zmieniłeś hasło z losowego w aplikacji na Androida (jeśli oczywiście masz tę bazę z urządzenia z Androidem).

## Zapytania SQL

Możesz wykonywać zapytania do `direct_messages`, `group_messages` i `all_messages` (lub ich prostszych odpowiedników `direct_messages_plain`, `group_messages_plain` i `all_messages_plain`), na przykład:

```sql
-- możesz umieścić te lub preferowane ustawienia w ~/.sqliterc
-- aby zachować ciągłość sesji klienta sqlite3
.mode column
.headers on
.nullvalue NULL

-- proste podglądy na wiadomości bezpośrednie, grupowe i all_messages
-- z deduplikacją wiadomości użytkownika dla grupy i all_messages;
-- tylko obsługa zdarzeń czatu "x.msg.new" ("nowa wiadomość") - filtruje zdarzenia serwisowe;
-- msg_sent ma wartość 0 dla odebranych, 1 dla wysłanych
select * from direct_messages_plain;
select * from group_messages_plain;
select * from all_messages_plain;

-- zapytanie o inne szczegóły historii czatu za pomocą zwykłego SQL, na przykład:
-- pliki, które zaoferowałeś do wysłania
select * from direct_messages where msg_sent = 1 and chat_msg_event = 'x.file';
-- wszystko co catherine wysłała związane z kotami
select * from direct_messages where msg_sent = 0 and contact = 'catherine' and msg_body like '%cats%';
-- cała korespondencja z Alice w #team
select * from group_messages where group_name = 'team' and contact = 'alice';

-- zbieranie danych z czatu
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
