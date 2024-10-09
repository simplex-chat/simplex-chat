---
title: Android uygulamasında dosyalara erişme
revision: 07.02.2023
---

| Son Güncelleme 07.02.2023 | Diiler: TR, [EN](/docs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md), [CZ](/docs/lang/cs/ANDROID.md), [PL](/docs/lang/pl/ANDROID.md) |

# Android uygulamasında dosyalara erişme

SimpleX veritabanlarını kullanır ve tercihlerinizi Android'deki özel bir veri dizini içinde saklar. Dizin şunları içerir:
- veritabanları
- gönderilen ve alınan dosyalar
- ihtiyaç duyulmadığında silinecek geçici dosyalar
- kullanıcı tercihleri.


SimpleX veri dizini içinde nelerin depolandığını görüntülemek istiyorsanız aşağıdakilere sahip olmanız gerekir:
- Unix tabanlı işletim sistemi (veya Windows üzerinde [MinGW](https://www.mingw-w64.org/downloads/))
- ADB (Android Debug Bridge) aracı yüklenmiş bir bilgisayar veya ([buradan indirin](https://developer.android.com/studio/releases/platform-tools) ve yükleyin)
- USB veya Wi-Fi ile bilgisayara bağlı olan cihazınız.

## Süreç:

- SimpleX'i açın, `Veritabanı parolası ve dışa aktarma`yı seçin ve `Uygulama veri yedeklemesi` seçeneğini etkinleştirin. Bu, diğer adımların çalışmasını sağlayacaktır
- _isteğe bağlı_: eğer veritabanı içeriğini görüntülemek istiyorsanız, veritabanı parolasını rastgele olanından kendi parolanız ile değiştirmelisiniz. Bunu yapmak için, `Veritabanı parolası ve dışa aktarma` ekranında bir sohbeti durdurun, `Veritabanı parolası` nı seçin, belirlediğiniz yeni parolayı girin, onaylayın ve ardından güncelleyin. Şifreyi unutmayın, aksi takdirde parolanın gerektiği durumlarda verilerinize erişimi kaybedeceksiniz.
- bir terminal emülatörü açın (Windows CMD/Powershell çalışmayacaktır) ve dizin olarak yedeklemek için kullanacağınız konumu seçiniz:

```bash
cd /tmp  # sadece bir örnek
```
Ardından aşağıdakileri sırasıyla çalıştırın:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

Şimdi cihazın kilidini açın ve şifreleme için herhangi bir parola kullanmadan yedekleme işlemini onaylayın, aksi takdirde komutlar çalışmayacaktır.

Bundan sonra yedekleme sonlanacaktır. Eğer `tar: Error is not recoverable: exiting now` şeklinde bir hata görmenize rağmen daha öncesinde bazı dosya adları çıktı olarak verilmişse endişelenmenize gerek yok, her şey yolunda.

Yedeklenen dosyalar artık `./apps/chat.simplex.app/` konumunda olacaktır.

SimpleX'in modern bir sürümünü kullanıyorsanız veritabanlarının şifreleneceğini ve `sqlcipher' uygulamasını kullanmadan ve şifre çözme parolasını bilmeden içeriğini görüntüleyemeyeceğinizi lütfen unutmayınız (öncelikle daha önce bahsedildiği gibi uygulamada rastgele oluşturulan parolayı kendinizinkiyle değiştirmeniz gerekir).

## Veritabanlarının şifresini çözme

Veritabanındaki verileri görüntülemek için önce şifrelerini çözmeniz gerekir. Favori paket yöneticinizi kullanarak `sqlcipher` aracını kurun ve veritabanlarının bulunduğu dizinde aşağıdaki komutları sırasıyla çalıştırın:

```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# İyi çalıştığından emin olun
select * from users;
```

Eğer `Parse error: no such table: users` uyarısını görürseniz, doğru parolayı girdiğinizden ve parolayı Android uygulamasında rastgele olarak değiştirdiğinizden emin olun (tabii ki bu veritabanını Android cihazdan aldıysanız).