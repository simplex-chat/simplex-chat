---
title: Katkıda bulunma rehberi
revision: 31.01.2023
---

| Son Güncelleme 31.01.2023 | Diller: TR, [EN](/docs/CONTRIBUTING.md), [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md) |

# Katkıda bulunma rehberi

## SQLCipher şifrelemesi etkinken derleme

OpenSSL başlıkları ile kütüphanelerinin konumu ve şifreleme modunu ayarlayan bayrak ile proje kök dizinine `cabal.project.local`'i ekleyin:

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## MacOS üzerinde OpenSSL

MacOS varsayılan olarak LibreSSL ile birlikte gelir, SimpleX'i kaynaktan derlemek için OpenSSL yüklenmelidir.

OpenSSL `brew install openssl@1.1` ile kurulabilir

İşlerin düzgün çalışması için `/opt/homebrew/opt/openssl@1.1/bin` adresini PATH'inize eklemeniz gerekecektir


## Proje dalları

**simplex-chat reposunda**

- `stable` - uygulamaların kararlı sürümü, önceki kararlı sürüme (GHC 9.6.3) güncellemeler için kullanılabilir.

- `stable-android` - Nix (GHC 8.10.7) ile kararlı Android çekirdek kütüphanesi oluşturmak için kullanılır - yalnızca Android armv7a için.

- `master` - beta sürüm sürümleri için dal (hem GHC 9.6.3 hem de 8.10.7 ile uyumlu).

- `master-android` - Nix (GHC 8.10.7) ile beta Android çekirdek kütüphanesi oluşturmak için kullanılır - yalnızca Android armv7a için.

**simplexmq reposunda**

- `master` - hem GHC 9.6.3 hem de 8.10.7 ile uyumludur.

## Geliştirme ve yayınlama süreci

1. PR'leri hem simplex-chat hem de simplexmq depoları için _sadece_ `master` dalına yapın.

2. Android, iOS ve Windows için çekirdek kütüphaneler oluşturmak için:
- `master` dalını `master-android` dalıyla merge edin.
- GitHub'a push edin.

3. Tüm kütüphaneler `master` dalından, Android armv7a - `master-android` dalından oluşturulmalıdır.

4. Masaüstü ve CLI uygulamaları oluşturmak için `master` dalında etiket (tag) oluşturun, APK dosyaları sürüme eklenmelidir.

5. App Store ve Play Store'da halka açık olarak yayınlandıktan sonra artık:
- `master`'ı `stable` dalı ile
- `master`'ı `master-android` dalı ile (ve kodu derleyin/güncelleyin)
- `master-android`'ı `stable-android` dalı ile birleştirebilirsiniz

6. Bağımsız olarak, simplexmq reposunun `master` dalı, kararlı sürümlerde `stable` dalıyla birleştirilmelidir.

## Branchlar ve PR'ler

PR adlarında ilk sözcük olarak değişiklik kapsamı (veya virgülle ayrılmış kapsamlar) kullanın ve ardından iki nokta üst üste işareti ekleyin. Commit adının kendisi şimdiki zamanda formunda ve küçük harfle yazılmalıdır.

simplex-chat reposundaki PR isimleri sürüm notlarında kullanılır ve bunlar değişikliği değil çözülen sorunu tanımlamalıdır. Olası PR kapsamları:
- ios
- android
- desktop
- core
- docs
- website
- ci

PR'leri squash ediyoruz, incelemeden sonra ilgili dal geçmişini yeniden yazmıyoruz.

Bazı karmaşık özellikler için, hazır olduklarında birleştirilecek özellik dalları oluşturuyoruz - doğrudan bunlara committe bulunmayın, PR'leri özellik dallarına yapın.

## GHC 8.10.7 ve GHC 9.6.3 arasındaki farklar

1. Temel fark `DuplicateRecordFields` uzantısı ile ilgilidir.

GHC 9.6.3'te seçiciler kullanılırken tür belirtmek artık mümkün değildir, bunun yerine GHC 8.10.7'de kaldırılması gereken OverloadedRecordDot uzantısı ve sözdizimi kullanılmaktadır:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- use this in GHC 9.6.3 when needed
{-# LANGUAGE OverloadedRecordDot #-}

-- GHC 9.6.3 syntax
let x = record.field

-- GHC 8.10.7 syntax removed in GHC 9.6.3
let x = field (record :: Record)
```

Kayıt güncelleme sözdizimini kullanırken tür belirtmek hala mümkündür, derleyici uyarısını bastırmak için bu pragmayı kullanın:

```haskell
-- use this in GHC 9.6.3 when needed
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. Çoğu monad fonksiyonunun artık belirli monad modüllerinden (örneğin `Control.Monad.Except`) değil, `Control.Monad`dan içe aktarılması gerekiyor.

```haskell
-- use this in GHC 9.6.3 when needed
import Control.Monad
```

[Bu PR] (https://github.com/simplex-chat/simplex-chat/pull/2975/files) tüm farklılıklara sahiptir.