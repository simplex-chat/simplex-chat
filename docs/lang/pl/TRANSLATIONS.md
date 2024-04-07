---
title: Współtworzenie tłumaczenia SimpleX Chat
revision: 19.03.2023
---

| 19.03.2023 | EN, [CZ](/docs/lang/cs/TRANSLATIONS.md), [FR](/docs/lang/fr/TRANSLATIONS.md), [PL](/docs/lang/pl/TRANSLATIONS.md) |

# Współtworzenie tłumaczenia SimpleX Chat

Dziękujemy za zainteresowanie się tłumaczeniem SimpleX Chat - to bardzo pomaga w uczynieniu go dostępnym dla szerszego grona użytkowników i naprawdę doceniamy Twoją pomoc.

Wymaga to znacznej inwestycji czasu - większość ludzi tego początkowo nie docenia - oraz stałej opieki w miarę rozwoju aplikacji.

Ten dokument został stworzony, po to by przyspieszyć ten proces i podzielić się kilkoma ważnymi "gafami", które odkryliśmy podczas pracy z Weblate - platformą, której używamy do tłumaczeń interfejsu.

## Zanim rozpoczniesz tłumaczenie

1. Utwórz konto w Weblate, używając tego samego adresu e-mail, którego używasz na platformie GitHub - dzięki temu Twój wkład będzie powiązany z kontem GitHub, co może okazać się dla Ciebie przydatne w niektórych przypadkach. Gdy tłumaczenie zostanie udostępnione użytkownikom, dodamy nazwę twojego konta do [listy tłumaczy] (https://github.com/simplex-chat/simplex-chat#translate-the-apps), chyba że poprosisz nas, abyśmy tego nie robili.

2. Przed rozpoczęciem tłumaczenia należy podpisać prostą umowę licencyjną za pośrednictwem Weblate - ma to na celu uniknięcie konfliktów związanych z prawami własności intelektualnej. Kopia tej umowy jest również [dostępna tutaj](https://github.com/simplex-chat/cla/blob/master/CLA.md).

3. Możemy również dodać Cię do grupy tłumaczy w przypadku jakichkolwiek pytań i aktualizacji - skontaktuj się z programistami za pośrednictwem czatu (po zainstalowaniu aplikacji lub później, poprzez "Wyślij pytania i pomysły" w ustawieniach aplikacji).

## Proces tłumaczenia

Najłatwiej jest najpierw przetłumaczyć aplikację na Androida, a dopiero później aplikację na iOS, ponieważ przetłumaczone ciągi Androidowej aplikacji są skonfigurowane jako słownik dla iOS.

Kroki są następujące:

1. [Tłumaczysz aplikację na Androida](#translating-android-app) w Weblate.
2. [Sprawdzamy i publikujemy tłumaczenia aplikacji na Androida](#releasing-android-app-translations).
3. Sprawdzasz tłumaczenia w aplikacji i poprawiasz ewentualne błędy.
4. [Tłumaczysz aplikację iOS w Weblate](#translating-ios-app).
5. Sprawdzamy i publikujemy tłumaczenia aplikacji iOS.

### Tłumaczenie aplikacji na Androida

1. Zacznij od [aplikacji na Androida](https://hosted.weblate.org/projects/simplex-chat/android/), zarówno podczas wykonywania najbardziej czasochłonnego tłumaczenia wstępnego, jak i dodawania ciągów później. Ze względu na to, że po pierwsze, ciągi w systemie iOS mogą pojawiać się w Weblate z pewnym opóźnieniem, ponieważ wymagają ręcznego zatwierdzenia z naszej strony, zanim będą widoczne, a po drugie, aplikacja na Androida jest skonfigurowana jako słownik dla aplikacji na iOS. 2/3 wszystkich ciągów wymaga tylko kliknięcia, aby przenieść je z Androida na iOS (nadal zajmuje to trochę czasu, Weblate niestety tego nie automatyzuje).

2. Niektóre ciągi nie wymagają tłumaczenia, ale nadal trzeba je skopiować - w interfejsie użytkownika weblate znajduje się odpowiedni przycisk:

<img src="./images/weblate_1.png" alt="weblate: copy source to translation" width="100%">

3. Weblate posiada również automatyczne sugestie, które mogą przyspieszyć ten proces. Czasami mogą być używane w niezmienionej formie, a czasami wymagają edycji - kliknij, aby użyć ich w tłumaczeniach.

4. Zwróć również uwagę na Klucz ciągu (znajduje się po prawej stronie ekranu) - może on dać ci podpowiedź, co ten ciąg oznacza, gdy jego znaczenie jest niejasne. Przykładowo, klucz dla " Dodatkowy akcent" ( nie wiadomo) to "color_primary_variant" (nieco bardziej jasne, że odnosi się do koloru używanego w aplikacji).

5. Gdy wszystkie ciągi w aplikacji na Androida zostaną przetłumaczone, przejrzyj je, aby zapewnić spójny styl i język, tak aby te same słowa były konsekwentnie używane do podobnych działań użytkownika, tak samo jak w języku angielskim. Czasami będziesz musiał użyć różnych słów w przypadkach, gdy angielski ma tylko jedno, spróbuj użyć tych wyborów spójnie w podobnych kontekstach, aby uprościć obsługę użytkownikom końcowym.

Prosimy również o sprawdzenie tłumaczeń przy użyciu przeglądarki Chrome i funkcji *Tłumacz na angielski* w trybie _Przeglądaj_ w weblate - tak będziemy sprawdzać tłumaczenia przed ich opublikowaniem. Popraw wszelkie błędy i dodaj komentarze w przypadkach, gdy uzasadnione jest użycie różnych tłumaczeń - znacznie przyspieszy to weryfikację.

### Udostępnianie tłumaczeń dla aplikacji na Androida

Gdy aplikacja na Androida zostanie przetłumaczona, poinformuj nas o tym.

My wtedy:
  - przejrzymy wszystkie tłumaczenia i zasugerujemy ewentualne poprawki - to również zajmie trochę czasu :)
  - scalimy je z kodem źródłowym - w tym czasie weblate będzie ustawiony na blokadę zmian.
  - stworzymy wersje beta aplikacji na iOS i Androida - możemy również dodać Cię do wewnętrznych grup testerów, abyś mógł zainstalować aplikacje przed innymi.
  - udostępnimy ją naszym użytkownikom korzystającym z wersji beta - już ponad tysiąc osób korzysta z wersji beta.
  - wydamy aplikację i uwzględnimy nowy język w ogłoszeniu.

### Tłumaczenie aplikacji iOS

1. Podczas tłumaczenia [aplikacji iOS](https://hosted.weblate.org/projects/simplex-chat/ios/) duża część ciągów jest dokładnie taka sama - można je skopiować jednym kliknięciem w sekcji słowniczka. Wskazówką jest podświetlenie całego ciągu źródłowego na żółto. Wiele innych ciągów jest bardzo do siebie podobnych, różnią się jedynie składnią lub sposobem pogrubienia czcionki - wymagają one minimalnej edycji. Istnieją jednak pewne ciągi które są unikalne dla platformy iOS - należy je przetłumaczyć osobno

2. Przejrzyj tłumaczenia na iOS w taki sam sposób jak na Androida i daj nam znać, kiedy będą gotowe do sprawdzenia - powtórzymy ten sam proces dla aplikacji na iOS.

Serdecznie dziękujemy! To ogromny wysiłek i wielka pomoc dla rozwoju sieci SimpleX.

<img src="./images/weblate_2.png" alt="weblate: automatic suggestions" width="100%">

## Częste błędy w tłumaczeniu

1. Słowo "chat" jest używane w kilku znaczeniach, w zależności od kontekstu. Może ono oznaczać "aplikację SimpleX Chat" (np. w opcji Rozpocznij/zatrzymaj czat) lub "pojedynczą rozmowę". Jeśli nie jest to jasne, zapytaj się nas, a my dodamy więcej uwag dotyczących tłumaczenia.

2. Prosimy o używanie liczby mnogiej i pojedynczej tak jak w oryginalnych ciągach, w przeciwnym razie może to zmienić ich znaczenie. Przykładowo, niektóre ustawienia mają zastosowanie do wszystkich kontaktów, a niektóre tylko do jednego kontaktu, będzie to mylące dla użytkownika, jeśli użyjesz liczby mnogiej w obu przypadkach.

3. Aplikacja używa "Passcode" do zapewnienia dostępu, a nie "hasła" ("password") - w wielu językach jest to tłumaczone jako "kod dostępu". Baza danych używa "Passphrase" - w wielu językach jest to tłumaczone jako "hasło". Prosimy o spójne używanie tych słów.

4. "Rola" użytkownika. To słowo odnosi się do zestawu uprawnień posiadanych przez użytkownika, może to być "właściciel", "administrator", "członek" lub "obserwator" (najniższe uprawnienie, które pozwala tylko na czytanie wiadomości i dodawanie reakcji na wiadomości). Tłumaczenie tego jako "tożsamość" lub "funkcja" może być nieprawidłowe.

5. "Moderate" / "moderated" ("moderować" / "zmoderowany"). Te słowa oznaczają odpowiednio "usunięcie wiadomości innego użytkownika" i "usunięcie przez administratora". Ta funkcja jest używana, gdy członek wysyła wiadomość, która nie jest odpowiednia dla grupy. Wiele języków ma podobne słowa.

## Jak sprawdzamy tłumaczenia

Aby zweryfikować poprawność tłumaczeń, sprawdzamy tłumaczenia poprzez przeglądanie stron Weblate w przeglądarce Google Chrome w trybie "Tłumacz na angielski". Na przykład, aby sprawdzić niemieckie tłumaczenia interfejsu Androida, ktoś z naszego zespołu przewinął [te 68 stron] (https://hosted.weblate.org/browse/simplex-chat/android/de/).

Nie oczekujemy, że odwrócone tłumaczenie będzie dokładnie takie samo jak oryginał, rzadko się to zdarza, ale że będzie ogólnie poprawne.

Znacznie ułatwiłoby to recenzję, gdybyś mógł wcześniej sprawdzić to w ten sam sposób i skomentować wszystkie przypadki, w których odwrócone tłumaczenia są zupełnie inne (mogą istnieć uzasadnione przypadki).

## Co dalej

1. W miarę aktualizowania aplikacji będziemy publikować aktualizacje w grupie tłumaczy. Nie masz absolutnie żadnego obowiązku tłumaczenia tych dodatkowych ciągów. Niemniej jednak bardzo docenimy, jeśli to zrobisz, ponieważ sprawia to, że użytkownicy mają o wiele lepsze wrażenia, gdy polegają na Twoich tłumaczeniach, niż gdyby jakaś nowa część aplikacji nie została przetłumaczona.

2. Możesz jeszcze bardziej pomóc w popularyzacji SimpleX w swoim kraju / grupie językowej, tłumacząc [naszą stronę internetową](https://simplex.chat) (również [przez weblate](https://hosted.weblate.org/projects/simplex-chat/website/)) i/lub [dokumenty GitHub](https://github.com/simplex-chat/simplex-chat/tree/master/docs/lang) (jest to możliwe tylko przez git)!

3. Ponadto, jeśli chcesz być moderatorem / administratorem grupy użytkowników w swoim języku, po przetłumaczeniu aplikacji możemy hostować taką grupę - przygotowujemy wytyczne dla społeczności i dodajemy kilka narzędzi moderacyjnych do aplikacji, która zostanie wydana w wersji 4.6 w marcu.


Jeszcze raz bardzo dziękujemy za pomoc w rozwoju SimpleX Chat!

Evgeny, założyciel SimpleX Chat.
