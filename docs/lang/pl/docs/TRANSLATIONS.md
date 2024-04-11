| Zaktualizowano 19.03.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/TRANSLATIONS.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/TRANSLATIONS.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/TRANSLATIONS.md) |

# Przyczynianie się do tłumaczenia SimpleX Chat

Ogromne podziękowania za zainteresowanie tłumaczeniem SimpleX Chat - to bardzo pomaga w udostępnieniu go szerszemu gronu użytkowników i naprawdę doceniamy waszą pomoc.

Wymaga to znacznej inwestycji czasowej - większość ludzi początkowo jej nie docenia - oraz bieżącej konserwacji, gdy rozwijamy aplikację.

Ten dokument został stworzony, aby przyspieszyć ten proces i podzielić się kilkoma ważnymi "gotchami", które odkryliśmy podczas pracy z Weblate - platformą, której używamy do tłumaczenia interfejsów.

## Zanim zaczniesz tłumaczyć

1. Załóż konto w Weblate, używając tego samego maila, którego używasz w GitHubie - to sprawi, że twój wkład będzie powiązany z twoim kontem na GitHubie i może ci pomóc w niektórych przypadkach. Gdy tłumaczenie zostanie udostępnione użytkownikom, dodamy nazwę Twojego konta do [listy tłumaczy](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#pomoc-w-tłumaczeniu-simplex-czat).

2. Przed rozpoczęciem tłumaczenia należy podpisać prostą umowę licencyjną z Weblate - ma to na celu uniknięcie wszelkich konfliktów wokół praw własności intelektualnej. Kopia tej umowy jest również [dostępna tutaj](https://github.com/simplex-chat/cla/blob/master/CLA.md).

3. Możemy również dodać Cię do grupy tłumaczy w razie jakichkolwiek pytań i aktualizacji - proszę połączyć się ze mną przez czat.

## Postępy w tłumaczeniu.

1. Proszę zacząć od [aplikacji na Android](https://hosted.weblate.org/projects/simplex-chat/android/), zarówno przy najbardziej czasochłonnym wstępnym tłumaczeniu, jak i dodawać wszelkie ciągi znaków później. Po pierwsze, ciągi z iOS mogą być nieco opóźnione od pojawienia się w Weblate, ponieważ wymaga to od nas ręcznego kroku, zanim będą widoczne. Po drugie, aplikacja na Androida jest ustawiona jako glosariusz dla aplikacji na iOS, a 2/3 wszystkich ciągów wymaga jedynie kliknięcia, aby przenieść je z Androida na iOS (nadal zajmuje to trochę czasu, Weblate nie automatyzuje tego, niestety).

2. Niektóre ciągi nie wymagają tłumaczenia, ale i tak trzeba je skopiować - służy do tego przycisk w weblate UI:

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/images/weblate_1.png" alt="weblate: kopiuj źródło do tłumaczenia" width="100%">.

3. Weblate ma również automatyczne sugestie, które mogą przyspieszyć proces. Czasami można je wykorzystać w takiej postaci, czasami wymagają pewnej edycji - kliknij, aby użyć ich w tłumaczeniach.

4. Gdy wszystkie ciągi w aplikacji na Androida są już przetłumaczone, przejrzyj je, aby zapewnić spójny styl i język, tak aby te same słowa były konsekwentnie używane dla podobnych działań użytkownika, tak samo jak w języku angielskim. Czasami trzeba będzie użyć różnych słów w przypadkach, gdy angielski ma tylko jeden, proszę spróbować użyć tych wyborów konsekwentnie w podobnych kontekstach, aby ułatwić użytkownikom końcowym.

5. Podczas tłumaczenia [aplikacji iOS](https://hosted.weblate.org/projects/simplex-chat/ios/) duża część ciągów znaków jest dokładnie taka sama - można je skopiować jednym kliknięciem w sekcji glosariusz. Wizualną podpowiedzią, że można to zrobić jest to, że cały ciąg źródłowy jest podświetlony na żółto. Wiele innych łańcuchów jest bardzo podobnych, różnią się jedynie składnią interpolacji lub sposobem użycia pogrubionej czcionki - wymagają minimalnej edycji. Istnieją pewne ciągi, które są unikalne dla platformy iOS - wymagają one osobnego tłumaczenia.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/images/weblate_2.png" alt="weblate: automatyczne sugestie" width="100%">

## Po zakończeniu tłumaczenia

Po przetłumaczeniu zarówno aplikacji na Androida, jak i iOS, prosimy o poinformowanie nas o tym.

Wówczas my:
  - przejrzeć wszystkie tłumaczenia i zasugerować ewentualne poprawki - to też zajmuje trochę czasu :)
  - scalimy je z kodem źródłowym - w czasie gdy to zrobimy weblate będzie zablokowany na zmiany.
  - stworzyć wersje beta aplikacji na iOS i Androida - możemy dodać Cię do wewnętrznej grupy testerów, abyś mógł zainstalować aplikację przed innymi.
  - udostępnić ją naszym użytkownikom w wersji beta - to już ponad tysiąc osób, które korzystają z naszych wersji beta.
  - wypuścić aplikację i uwzględnić nowy język w ogłoszeniu.

*Zwróć uwagę*: staramy się zachować spójność funkcji aplikacji między platformami Android i iOS, jeśli to możliwe, więc wydamy i ogłosimy nowy język, gdy obie platformy zostaną przetłumaczone. Nie oznacza to, że musisz to zrobić, ale będziemy musieli poczekać, aż ktoś inny przetłumaczy drugą platformę. Ale jeśli zaczniesz od Androida, to dodanie iOS zajmuje zwykle 3-4x mniej czasu.

## Jak sprawdzamy tłumaczenia

Aby zatwierdzić poprawność tłumaczeń przeglądamy tłumaczenia wsteczne przeglądając strony Weblate w przeglądarce Google Chrome w trybie "Tłumacz na angielski". Na przykład, aby przejrzeć niemieckie tłumaczenia interfejsu Androida ktoś z naszego zespołu przewinął [te 49 stron](https://hosted.weblate.org/browse/simplex-chat/android/de/).

Nie szukamy odwrotnego tłumaczenia będącego dokładnie tym samym co oryginał, rzadko się to zdarza, tylko że jest ono ogólnie poprawne.

Znacznie ułatwilibyście przegląd, gdybyście mogli przejrzeć go z góry w ten sam sposób i skomentować wszelkie przypadki, w których odwrotne tłumaczenia są zupełnie inne (mogą być na to uzasadnione przypadki).

## Co dalej

1. W miarę aktualizacji aplikacji, zamieszczamy aktualizacje w grupie tłumaczy. Nie masz absolutnie żadnego obowiązku tłumaczenia tych dodatkowych ciągów znaków. Bardzo doceniamy, jeśli jednak to zrobisz, ponieważ sprawia to, że doświadczenia użytkowników są o wiele lepsze, gdy polegają na twoich tłumaczeniach, niż gdyby jakaś nowa część aplikacji nie została przetłumaczona.

2. Możesz dodatkowo pomóc w przyjęciu SimpleX w swoim kraju / grupie językowej, tłumacząc [naszą stronę internetową](https://simplex.chat/pl) (również [poprzez weblate](https://hosted.weblate.org/projects/simplex-chat/website/)) i/lub [dokumenty GitHub](https://github.com/simplex-chat/simplex-chat/tree/master/docs/lang) (jest to możliwe tylko poprzez git)!

3. Ponadto, jeśli chcesz być moderatorem/adminem grupy użytkowników w swoim języku, po przetłumaczeniu aplikacji możemy prowadzić taką grupę - przygotowujemy wytyczne dla społeczności i dodajemy kilka narzędzi moderacyjnych do aplikacji, która zostanie wydana w wersji 4.6 w marcu.


Jeszcze raz bardzo dziękujemy za pomoc w rozwoju SimpleX Chat!

Evgeny, założyciel SimpleX Chat.
