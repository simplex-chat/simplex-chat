| Aktualizováno 26.02.2023 | Jazyky: CZ, [EN](/docs/SQL.md), [FR](/docs/lang/fr/SQL.md) |

# Přístup ke zprávám v databázi

## Dešifrování databází

Chcete-li zobrazit data v databázi, musíte je nejprve dešifrovat. Nainstalujte `sqlcipher` pomocí svého oblíbeného správce balíčků a v adresáři s databázemi spusťte následující příkazy:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Ujistěte se, že vše funguje správně
select * from users;
```

Pokud se zobrazí `Parse error: no such table: users`, ujistěte se, že jste zadali správnou přístupovou frázi a že jste ji v aplikaci pro Android změnili z náhodné (pokud jste tuto databázi získali ze zařízení s Androidem, samozřejmě).

## SQL dotazy

Můžete spouštět dotazy proti `direct_messages`, `group_messages` a `all_messages` (nebo jejich jednodušším alternativám `direct_messages_plain`, `group_messages_plain` a `all_messages_plain`), např:

```sql
-- tato nebo vámi preferovaná nastavení můžete vložit do souboru ~/.sqliterc
-- aby přetrvaly napříč relacemi klienta sqlite3
.mode column
.headers on
.nullvalue NULL

-- jednoduché pohledy na direct, group a all_messages
-- s deduplikací uživatelských zpráv pro group a all_messages;
-- pouze události chatu 'x.msg.new' ("nová zpráva") - filtruje události služby;
-- msg_sent je 0 pro přijaté, 1 pro odeslané
select * from direct_messages_plain;
select * from group_messages_plain;
select * from all_messages_plain;

-- dotaz na další podrobnosti historie chatu pomocí běžného SQL, například:
-- soubory, které jste nabídli k odeslání
select * from direct_messages where msg_sent = 1 and chat_msg_event = 'x.file';
-- vše, co catherine poslala v souvislosti s kočkami
select * from direct_messages where msg_sent = 0 and contact = 'catherine' and msg_body like '%cats%';
-- veškerá korespondence s alice v #teamu
select * from group_messages where group_name = 'team' and contact = 'alice';

-- shrňte data z chatu
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
