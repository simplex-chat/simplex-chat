---
title: Přístup k souborům v aplikaci Android
revision: 07.02.2023
---
| Aktualizováno 07.02.2023 | Jazyky: CZ, [EN](/docs/ANDROID.md) |

# Přístup k souborům v aplikaci Android

SimpleX používá databáze a ukládá své předvolby do soukromého datového adresáře v systému Android. Adresář obsahuje:
- Databáze
- odeslané a přijaté soubory
- dočasné soubory, které budou smazány, když nebudou potřeba
- uživatelské předvolby.


Pokud chcete zobrazit, co je uloženo v datovém adresáři SimpleX, musíte mít:
- Unixový operační systém (nebo [MinGW](https://www.mingw-w64.org/downloads/) na Windows).
- Nástroj ADB (Android Debug Bridge) nainstalovaný v počítači ([stáhněte si jej zde](https://developer.android.com/studio/releases/platform-tools) a nainstalujte).
- zařízení připojené přes USB nebo Wi-Fi k počítači.

## Postup:

- Otevřete SimpleX, přejděte na `Databáze passphrase & export`, povolte `Zálohování dat aplikace`. Tím se zprovozní další kroky
- _Volitelné_: pokud chcete zobrazit obsah databáze, změňte přístupovou frázi databáze z náhodné na svou. Chcete-li to provést, zastavte chat na obrazovce `Database passphrase & export`, otevřete `Database passphrase`, zadejte novou passphrase a potvrďte ji, poté ji aktualizujte. Nezapomeňte ji, jinak přijdete o všechna svá data v případě, že bude passphrase později znovu požadována.
- otevřete emulátor terminálu (Windows CMD/Powershell nebude fungovat) a změňte adresář na ten, který chcete použít pro uložení zálohy:

```bash
cd /tmp # jen příklad
```
Poté spusťte následující příkaz:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

Nyní odemkněte zařízení a potvrďte operaci zálohování bez použití hesla pro šifrování, jinak příkazy nebudou fungovat.

Poté by mělo být zálohování ukončeno. Pokud se zobrazí chybové hlášení `tar: Error is not recoverable: exiting now`, ale předtím jste vypsali názvy některých souborů, nebojte se, je to v pořádku.

Nyní budou zálohované soubory uvnitř `./apps/chat.simplex.app/`.

Upozorňujeme, že pokud používáte moderní verzi SimpleX, budou databáze zašifrované a jejich obsah nebudete moci zobrazit bez použití aplikace `sqlcipher` a bez znalosti dešifrovací fráze (musíte ji nejprve změnit na svou z náhodně vygenerovaných v aplikaci).

## Dešifrování databází

Chcete-li zobrazit data v databázi, musíte je nejprve dešifrovat. Nainstalujte `sqlcipher` pomocí svého oblíbeného správce balíčků a v adresáři s databázemi spusťte následující příkazy:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Ujistěte se, že vše funguje správně
select * from users;
```

Pokud se zobrazí `Parse error: no such table: users`, ujistěte se, že jste zadali správnou přístupovou frázi a že jste ji v aplikaci pro Android změnili z náhodné (pokud jste tuto databázi získali ze zařízení s Androidem, samozřejmě).
