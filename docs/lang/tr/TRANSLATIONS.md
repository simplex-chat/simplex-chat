---
title: SimpleX Chat'e çevirilerle katkıda bulunma
revision: 19.03.2023
---

| Son Güncellenme 19.03.2023 | Diller: TR, [EN](/docs/TRANSLATIONS.md), [FR](/docs/lang/fr/TRANSLATIONS.md), [CZ](/docs/lang/cs/TRANSLATIONS.md), [PL](/docs/lang/pl/TRANSLATIONS.md) |

# SimpleX Chat'e çevirilerle katkıda bulunma

SimpleX Chat'i çevirmeye gösterdiğiniz ilgi için çok teşekkür ederiz. Bu daha geniş bir kullanıcı kitlesi için erişilebilir hale gelmesine çok yardımcı oluyor ve yardımınız için gerçekten minnettarız.

Bu önemli bir zaman yatırımı gerektirir ki çoğu insan başlangıçta bunu hafife alır ve uygulamayı geliştirdikçe sürekli bakım gerektirir.

## Çeviriye başlamadan önce

1. GitHub'da kullandığınız e-postayı kullanarak Weblate'te bir hesap oluşturun - bu, katkılarınızı GitHub hesabınızla ilişkilendirir ve bazı durumlarda size yardımcı olabilir. Çeviri kullanıcılara sunulduğunda, bizden yapmamamızı istemediğiniz sürece hesap adınızı [çevirmenler listesine](https://github.com/simplex-chat/simplex-chat#translate-the-apps) ekleyeceğiz.

2. Çeviriye başlamadan önce Weblate üzerinden imzalamanız gereken basit bir katılımcı lisans sözleşmesi vardır - bu, fikri mülkiyet haklarıyla ilgili herhangi bir çatışmayı önlemek içindir. Bu anlaşmanın bir kopyası da [burada](https://github.com/simplex-chat/cla/blob/master/CLA.md) mevcuttur.

3. Ayrıca sorularınız ve güncellemeleriniz için sizi çevirmenler grubuna ekleyebiliriz - lütfen geliştiricilerle sohbet yoluyla bağlantı kurun (uygulamayı yeni yüklediğinizde veya daha sonra, uygulama ayarlarındaki "Soru ve fikir gönder" aracılığıyla).

## Çeviri süreci

Android uygulama dizeleri iOS için bir sözlük olarak ayarlandığından, önce Android uygulamasını ve ardından iOS uygulamasını çevirmek daha kolaydır.

Adımlar şunlardır:

1. Weblate'te [Android uygulamasını çevirirsiniz](#translating-android-app).
2. [Android uygulama çevirilerini gözden geçirir ve yayınlarız](#releasing-android-app-translations).
3. Uygulamadaki çevirileri gözden geçirir ve hataları düzeltirsiniz.
4. [Weblate'te iOS uygulamasını çevirirsiniz](#translating-ios-app)
5. iOS uygulama çevirilerini inceler ve yayınlarız.

### Android uygulamasını çevirme

1. Lütfen en çok zaman alan ilk çeviriyi yaparken [Android uygulaması](https://hosted.weblate.org/projects/simplex-chat/android/) adresinden başlayın ve dizeleri daha sonra ekleyin. İlk olarak, iOS dizelerinin Weblate'te görünmesi biraz gecikebilir, çünkü görünür hale gelmeden önce bizim tarafımızdan manuel bir adım atılması gerekir. İkincisi, Android uygulaması iOS uygulaması için bir sözlük olarak ayarlanmıştır ve tüm dizelerin 2/3'ü Android'den iOS'a aktarmak için sadece tıklama gerektirir (yine de biraz zaman alır, Weblate maalesef bunu otomatikleştirmez).

2. Bazı dizelerin çeviriye ihtiyacı yoktur, ancak yine de kopyalanmaları gerekir - bunun için weblate kullanıcı arayüzünde bir düğme vardır:

<img src="./images/weblate_1.png" alt="weblate: copy source to translation" width="100%">

3. Weblate ayrıca süreci hızlandırabilecek otomatik önerilere de sahiptir. Bazen olduğu gibi kullanılabilirler, bazen de biraz düzenlemeye ihtiyaç duyarlar, çevirilerde kullanmak için bunlara tıklayın.

4. Ayrıca, lütfen Anahtar dizesine dikkat edin (ekranın sağındadır) - net olmadığında bu dizenin ne anlama geldiği hakkında size bir ipucu verebilir. Örneğin, "Ek vurgu" için anahtar (net değil) "color_primary_variant" (uygulamada kullanılan bir renge atıfta bulunduğu biraz daha açık).

5. Android uygulamasındaki tüm dizeler çevrildikten sonra, lütfen tutarlı bir stil ve dil sağlamak için gözden geçirin, böylece İngilizce'de olduğu gibi benzer kullanıcı eylemleri için aynı kelimeler tutarlı bir şekilde kullanılır. Bazen, İngilizcede sadece bir tane olan durumlarda farklı kelimeler kullanmanız gerekecektir, lütfen son kullanıcıların işini kolaylaştırmak için bu seçenekleri benzer bağlamlarda tutarlı bir şekilde kullanmaya çalışın.

Lütfen Chrome tarayıcısını ve weblate'in _Browse_ modundaki *Translate to English* özelliğini kullanarak ters çevirileri de inceleyin - çeviriler yayınlanmadan önce inceleyeceğimiz şey budur. Herhangi bir hatayı düzeltin ve yeterince farklı çevirilerin haklı olduğu durumlarda lütfen yorum ekleyin - bu, incelemeyi çok daha hızlı hale getirecektir.

### Android uygulama çevirilerinin yayınlanması

Android uygulaması çevrildikten sonra lütfen bize bildirin.

O zaman şunları yapacağız:
  - tüm çevirileri gözden geçirip, gerekirse herhangi bir düzeltme önereceğiz - ki bu da biraz zaman alır :)
  - bunları kaynak kodla birleştireceğiz - biz bunu yaparken weblate değişiklikler için kilitli olacak.
  - Hem iOS hem de Android uygulamalarının beta sürümlerini oluşturun - sizi dahili test gruplarına da ekleyebiliriz, böylece uygulamaları herkesten önce yükleyebilirsiniz.
  - Beta kullanıcılarımız için sürümün yayımlanması - beta sürümlerimizi kullanan binden fazla kişi var.
  - uygulamayı yayınlayıp ve yeni dili duyuruya ekleyeceğiz.

### iOS uygulamasını çevirme

1. [iOS uygulamasını](https://hosted.weblate.org/projects/simplex-chat/ios/) çevirdiğinizde, dizelerin büyük bir kısmı tamamen aynıdır - sözlük bölümünde tek bir tıklamayla kopyalanabilirler. Bunun yapılabileceğine dair görsel ipucu, tüm kaynak dizesinin sarı renkle vurgulanmasıdır. Diğer birçok dizgi çok benzerdir, yalnızca enterpolasyon sözdiziminde veya kalın yazı tipinin nasıl kullanıldığında farklılık gösterirler - minimum düzenleme gerektirirler. iOS platformuna özgü bazı dizeler vardır - bunların ayrıca çevrilmesi gerekir

2. Lütfen iOS çevirilerini Android ile aynı şekilde inceleyin ve incelemeye hazır olduğunda bize bildirin - iOS uygulaması için aynı süreci tekrarlayacağız.

Çok teşekkürler! Bu büyük bir çaba ve SimpleX Network'ün büyümesi için büyük bir yardım.

<img src="./images/weblate_2.png" alt="weblate: automatic suggestions" width="100%">

## Yaygın çeviri hataları

1. "Sohbet" kelimesi, bağlama bağlı olarak çeşitli anlamlarda kullanılmaktadır. "SimpleX Chat uygulaması" (örneğin Sohbeti başlat/durdur'da) veya "tek bir konuşma" anlamına gelebilir. Net olmadığında lütfen sorun, daha fazla çeviri notu ekleyeceğiz.

2. Lütfen orijinal dizelerde olduğu gibi çoğul ve tekil kullanın, aksi takdirde anlam değişebilir. Örneğin, bazı ayarlar tüm kişiler için, bazıları ise sadece bir kişi için geçerlidir, her iki durumda da çoğul kullanırsanız kafa karıştırıcı olacaktır.

3. Uygulama, erişim sağlamak için "şifre" değil "Parola" kullanır - birçok dilde "erişim kodu" olarak çevrilir. Veritabanı "Passphrase" kullanır - birçok dilde "Password" olarak çevrilir. Lütfen bu kelimeleri tutarlı bir şekilde kullanın.

4. Üye "rolü". Bu kelime kullanıcının sahip olduğu izinler kümesini ifade eder, "sahip", "yönetici", "üye" veya "gözlemci" (yalnızca mesajları okumaya ve mesaj tepkileri eklemeye izin veren en düşük izin) olabilir. Bunu "kimlik" veya "işlev" olarak çevirmek yanlış olabilir.

5. "Modere"/"moderated". Bu kelimeler sırasıyla "başka bir üyenin mesajını silmek" ve "yönetici tarafından silindi" anlamına gelir. Bu özellik, bir üye grup için uygun olmayan bir mesaj gönderdiğinde kullanılır. Birçok dilde benzer kelimeler vardır.

## Çevirileri nasıl gözden geçiriyoruz

Çevirilerin doğruluğunu onaylamak için Google Chrome tarayıcısında "İngilizceye Çevir" modunda Weblate sayfalarına göz atarak ters çevirileri gözden geçiriyoruz. Örneğin, Android arayüzünün Almanca çevirilerini incelemek için ekibimizden biri [bu 68 sayfa](https://hosted.weblate.org/browse/simplex-chat/android/de/) arasında gezindi 

Ters çevirinin orijinaliyle tamamen aynı olmasını aramıyoruz, bu nadiren olur, sadece genel olarak doğru olmasını istiyoruz.

Önceden aynı şekilde gözden geçirebilirseniz ve ters çevirilerin tamamen farklı olduğu durumlar hakkında yorum yapabilirseniz (bunun için geçerli durumlar olabilir) incelemeyi çok daha kolaylaştırırsınız.

## Sırada ne var

1. Uygulamayı güncelledikçe, güncellemeleri çevirmenler grubunda yayınlıyoruz. Bu ek dizeleri çevirmek için kesinlikle hiçbir yükümlülüğünüz yoktur. Yine de bunu yaparsanız çok memnun oluruz, çünkü kullanıcıların sizin çevirilerinize güvenmesi, uygulamanın yeni bir bölümünün çevrilmemesinden çok daha iyi bir deneyim sağlar.

2. SimpleX'in ülkenizde/dil grubunuzda benimsenmesine daha fazla yardımcı olabilmek için [Web sitemizi](https://simplex.chat) (ayrıca [weblate aracılığıyla](https://hosted.weblate.org/projects/simplex-chat/website/)) ve/veya [GitHub belgelerini](https://github.com/simplex-chat/simplex-chat/tree/master/docs/lang) (bu yalnızca git aracılığıyla mümkündür) çevirebilirsiniz.

3. Ayrıca, kendi dilinizde bir kullanıcı grubunun moderatörü/yöneticisi olmak istiyorsanız, uygulama çevrildikten sonra böyle bir gruba ev sahipliği yapabiliriz. Topluluk yönergelerini hazırlıyoruz ve Mart ayında v4.6'da yayınlanacak ve uygulamaya bazı moderasyon araçları ekleyeceğiz.

SimpleX Chat'i büyütmemize yardımcı olduğunuz için tekrar çok teşekkür ederiz!

Evgeny, SimpleX Chat kurucusu.