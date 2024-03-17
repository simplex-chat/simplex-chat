---
layout: layouts/article.html
title: "Instant notifications for SimpleX Chat mobile apps"
date: 2022-04-04
preview: Design of private instant notifications on Android and for push notifications for iOS.
permalink: "/blog/20220404-simplex-chat-instant-notifications.html"
---

# Natychmiastowe powiadomienia dla aplikacji mobilnych SimpleX Chat

**Opublikowano:** 4 kwietnia 2022 r.

## SimpleX Chat to pierwsza platforma czatowa, która z założenia jest w 100% prywatna - nie ma dostępu do Twoich połączeń

Odkąd wydaliśmy aplikacje mobilne SimpleX Chat kilka tygodni temu, mieliśmy wiele emocji od naszych użytkowników - prawie 2000 osób pobrało aplikację po [ogłoszeniu wydania](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220308-simplex-chat-mobile-apps.md)!

Ogromne podziękowania dla każdego, kto pobrał i połączył się z nami poprzez czat - było wiele świetnych pytań i sugestii, a w niektóre dni spędziłem większość czasu na czacie z naszymi użytkownikami :)

Od czasu wydania aplikacji, dodaliśmy i wydaliśmy:

- wsparcie dla iPhone 7.
- konfigurowalne serwery SimpleX.
- odpowiedzi, edycję i usuwanie wiadomości.
- zdjęcia profilowe.
- oraz, co najważniejsze, powiadomienia o prywatnych wiadomościach na urządzeniach z systemem Android - więcej o tym poniżej.

## Zainstaluj aplikacje i nawiąż prywatne połączenie!

Po zainstalowaniu aplikacji możesz połączyć się z każdym:

1. Utwórz swój lokalny profil czatu - nie jest on udostępniany serwerom SimpleX. Jest on lokalny dla Waszych urządzeń i będzie udostępniany Waszym kontaktom dopiero w momencie połączenia.
2. Aby nawiązać prywatne połączenie, musisz stworzyć jednorazowy link do połączenia lub kod QR poprzez przycisk "Dodaj kontakt" w aplikacji. Możesz pokazać kod QR swojemu kontaktowi osobiście lub za pośrednictwem połączenia wideo - jest to najbezpieczniejszy sposób utworzenia połączenia - lub możesz udostępnić łącze za pośrednictwem dowolnego innego kanału. Tylko jeden użytkownik może połączyć się za pośrednictwem tego linku.
3. Gdy inny użytkownik zeskanuje kod QR lub otworzy aplikację za pośrednictwem linku, połączenie zostanie utworzone i będziesz mógł wysyłać prywatnie zaszyfrowane wiadomości end-to-end, bez niczyjej wiedzy, że jesteś połączony.

Zobacz [filmik demonstracyjny](https://youtu.be/rZeVhsv_JAY), który pokazuje jak dwóch użytkowników łączy się i wysyła pierwsze wiadomości.

## Dlaczego to robimy

Budujemy SimpleX Chat, ponieważ wierzymy, że prywatność jest podstawowym prawem człowieka, a ochrona naszej osobistej sieci kontaktów jest nawet ważniejsza niż treść wiadomości - udostępnianie tej sieci może prowadzić do różnych negatywnych konsekwencji, od manipulowania nami w celu kupowania towarów, których nie potrzebujemy, manipulowania procesami wyborczymi, a w niektórych przypadkach ścigania niewinnych ludzi. Na przykład [Mohamedou Ould Salahi](https://en.wikipedia.org/wiki/Mohamedou_Ould_Slahi) był przetrzymywany w więzieniu Guantanamo przez 15 lat po jednym "niewłaściwym" telefonie. Jego historia została opowiedziana w jego pamiętniku oraz w filmie Mauretańczyk (2021) .

## Problem - użytkownicy oczekują natychmiastowego powiadomienia o nadejściu wiadomości!

Nasi pierwsi użytkownicy zorientowali się, że to, co w aplikacjach do przesyłania wiadomości uważamy za oczywiste - natychmiastowe powiadomienia o wiadomościach - brakuje w naszym pierwszym wydaniu aplikacji SimpleX Chat. Całkiem sporo osób myślało, że to raczej błąd, niż brakująca funkcja. Przepraszamy za rozczarowanie!

## Dlaczego nie możemy po prostu zrobić tego, co robi Messenger X?

SimpleX Chat to pierwszy i jedyny znany nam komunikator, który działa bez jakichkolwiek tożsamości użytkowników. Nie ma numerów telefonów, e-maili, nazw użytkownika, kluczy publicznych ani żadnych innych adresów czy identyfikatorów, które jednoznacznie identyfikowałyby użytkowników z siecią lub serwerami. Dlatego mówimy, że jest w 100% prywatny z założenia i zasadniczo różni się od innych platform czatowych.

Zamiast tego SimpleX Chat przypisuje te identyfikatory do jednokierunkowych kolejek wiadomości. To, co dla użytkowników SimpleX Chat wygląda jak kontakty i grupy [1], dla serwerów SimpleX wygląda jak niezorganizowany i niepowiązany zbiór jednokierunkowych kolejek wiadomości. Nasze serwery nie wiedzą, które kolejki należą do których użytkowników, kontaktów czy grup. Nawet pojedyncza rozmowa może odbywać się za pośrednictwem dwóch różnych serwerów (jeden dla wiadomości wysyłanych, drugi dla odbieranych). Dzięki temu nasza osobista sieć kontaktów jest niewidoczna dla serwerów.

Ale stwarza to też problem dla błyskawicznych powiadomień - wszystkie usługi powiadomień push wymagają posiadania tokena urządzenia.

Jak więc możemy działać bez tożsamości i nadal mieć błyskawiczne powiadomienia?

[1] tak, mamy grupy w naszej aplikacji terminalowej, a UI do zarządzania nimi pojawi się wkrótce w aplikacjach mobilnych. Niektórzy użytkownicy już dowiedzieli się jak [tworzyć grupy przez konsolę czatu](https://medium.com/@vsevolod.mineev/how-to-collaborate-across-multiple-devices-whilst-protecting-your-metadata-371af87d0ba0).

## We've cracked it for Android!

Po kilku badaniach nad tym, jak działają powiadomienia push na Androidzie i open-source'owe alternatywy dla powiadomień Google push, odkryliśmy, jak uniknąć dzielenia się tokenami urządzenia z jakimikolwiek serwerami.

Zaimplementowaliśmy odbieranie wiadomości jako usługę tła (w terminologii Androida, "usługę pierwszoplanową" pokazującą ikonę powiadomienia, gdy usługa jest uruchomiona) podążając za tym samym projektem co [ntfy.sh](https://github.com/binwiederhier/ntfy-android) stworzonym przez [Philippa Heckela](https://github.com/binwiederhier), który z kolei przypisuje projekt do [wpisu Roberto Huertasa](https://robertohuertas.com/2019/06/29/android_foreground_services/). Wielkie podziękowania dla nich!

Jak to działa? Kiedy aplikacja jest po raz pierwszy uruchomiona na urządzeniu z systemem Android, uruchamia usługę w tle, która utrzymuje połączenia TCP z serwerami wiadomości otwarte prawie bez ruchu (tylko okresowo sprawdza, czy połączenia nadal istnieją). To zużywa tylko kilka procent baterii na dzień, w zależności od tego, jak stabilne jest połączenie z Internetem, i dostarcza powiadomienia o wiadomościach, jak tylko przyjdą wiadomości.

Ta usługa nadal działa, nawet, gdy aplikacja jest wyłączona. Jest ona ponownie uruchamiana w momencie, gdy urządzenie zostało zrestartowane, nawet jeśli aplikacja nie zostanie otwarta - więc powiadomienia o wiadomościach przychodzą natychmiast za każdym razem. Aby maksymalnie wydłużyć czas pracy baterii, można ją wyłączyć, wyłączając opcję "Prywatne powiadomienia". Nadal będziesz otrzymywać powiadomienia, gdy aplikacja jest uruchomiona lub w tle.

Tak więc, dla Androida możemy teraz dostarczyć natychmiastowe powiadomienia o wiadomościach bez naruszania prywatności użytkowników w jakikolwiek sposób. Aplikacja w wersji 1.5, która zawiera prywatne powiadomienia natychmiastowe jest już dostępna w [Sklepie Play](https://play.google.com/store/apps/details?id=chat.simplex.app), w naszym [F-Droid repo](https://app.simplex.chat/) i poprzez bezpośrednie [APK](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk) pobrania!

Daj nam znać, co trzeba poprawić - to dopiero pierwsza wersja błyskawicznych powiadomień dla Androida!

## Nasze podejście do systemu iOS ma jeden kompromis

iOS jest znacznie bardziej zabezpieczony przed tym, jakie aplikacje mogą być uruchamiane na urządzeniach i rozwiązanie, które działało na Androidzie, nie jest możliwe do zastosowania na iOS.

Mamy już w aplikacji na iOS odświeżanie w tle, które okresowo sprawdza, czy nie ma nowych wiadomości i jeśli korzystacie z aplikacji codziennie, to dostarcza ona powiadomienia w ciągu 10 lub 20 minut. Nie jest to natychmiastowe, ale może być użyteczne dla niektórych. Jeśli jednak używasz aplikacji rzadko, to opóźnienie może stać się kilka godzin, lub telefon może przestać sprawdzać nowe wiadomości całkowicie. To nie jest idealne rozwiązanie!

Jedynym znanym nam rozwiązaniem jest wykorzystanie usługi powiadomień push (APN) firmy Apple do dostarczania powiadomień push.

Zaplanowaliśmy to, więc dodaliśmy do [SMP v1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220112-simplex-chat-v1-released.md) (protokół używany przez nasze serwery) rozszerzenie pozwalające klientowi na subskrybowanie powiadomień z kolejek wiadomości, poprzez osobne adresy kolejek i przy użyciu osobnych kluczy kryptograficznych dla każdej kolejki. Musi to być włączone przez klienta dla każdej kolejki osobno. Do tej pory nie korzystaliśmy z tego rozszerzenia, a teraz budujemy na jego podstawie usługę powiadomień SimpleX.

Jeśli użytkownik włączy powiadomienia push, to dla każdego kontaktu aplikacja włączy subskrypcję powiadomień i przekaże referencje do serwera powiadomień wraz z tokenem urządzenia wymaganym do dostarczenia powiadomień push na urządzenie użytkownika.

Serwer powiadomień będzie subskrybował te powiadomienia z serwerów SMP. Powiadomienia nie zawierają żadnej treści wiadomości, a jedynie sygnał, że wiadomość dotarła do serwera. Serwer powiadomień może wysłać na urządzenie tylko 2-3 ukryte powiadomienia na godzinę. Powiadomienie jest szyfrowane end-to-end i zawiera informację o tym, na którym serwerze znajduje się wiadomość, dzięki czemu klient może połączyć się z serwerem, pobrać i odszyfrować wiadomość oraz pokazać powiadomienie użytkownikom zawierające nazwę nadawcy i treść wiadomości. Żadna z tych informacji nie jest udostępniana żadnemu z serwerów.

Jeśli użytkownik otrzyma więcej niż 2-3 wiadomości na godzinę, serwer powiadomień może wysłać dodatkowe widoczne powiadomienia, które po prostu powiedzą "masz nową wiadomość", a użytkownik będzie musiał otworzyć aplikację, aby otrzymać i zobaczyć te wiadomości. Badamy również, czy możemy użyć powiadomień "mutable-content", które pozwalają wykonać pewne przetwarzanie, gdy powiadomienie nadejdzie, zanim pokaże je użytkownikom.

Jest to znaczna ilość prac rozwojowych, mamy na celu wydanie jej jeszcze w tym miesiącu.

Ten projekt jest kompromisem między prywatnością a wygodą. Serwer powiadomień będzie musiał mieć token urządzenia, aby dostarczyć powiadomienia. Kilka rzeczy, które zrobiliśmy (lub planujemy zrobić), aby poprawić ten kompromis:

1. Serwer powiadomień będzie przechowywał tokeny urządzeń i adresy kolejek tylko w pamięci, co czyni go bardziej skomplikowanym w dostępie dla potencjalnego napastnika. Jeśli serwer musiałby zostać ponownie uruchomiony, straciłby wszystkie skonfigurowane subskrypcje powiadomień i klienci musieliby tworzyć je ponownie. Zaprogramujemy klientów, aby okresowo sprawdzali istnienie subskrypcji powiadomień na serwerze powiadomień.
2. Serwer notificaiton nie będzie znał adresów kolejek wiadomości używanych do odbierania lub wysyłania wiadomości. Inny adres jest używany do subskrypcji powiadomień. Tak więc, podczas gdy serwer powiadomień miałby wiedzę o tym, ile kolejek ma Twoje urządzenie (i na jakich serwerach), nadal nie będzie wiedział, kto wysyła Ci wiadomości.
3. Planujemy również podzielić logikę subskrypcji powiadomień i dostarczania powiadomień do urządzeń na dwa różne serwery. Serwer, który subskrybuje powiadomienia, mógłby być samodzielnie hostowany, pozwalając na pełną kontrolę nad tym, jak go wdrożyć. Tylko ten serwer wiedziałby, jakich serwerów przesyłania wiadomości używasz lub ile masz kolejek przesyłania wiadomości. Serwer dostarczający powiadomienia do urządzeń będzie zarządzany przez SimpleX Chat, ponieważ musimy autoryzować go za pomocą usługi powiadomień push firmy Apple. Ten podział nie będzie dostępny w pierwszym wydaniu. Planujemy dodać go nieco później.

Tak więc, po dodaniu serwerów powiadomień, nasz projekt sieci będzie wyglądał tak:

```
  Twoje urządzenie iOS              Internet                        Serwery
----------------------- |   ------------------------   |   -------------------------
                        |                              |    (aktualnie może być
                        |                              |    samodzielnie hostowany)
+----------------+      |                              |       +----------------+
| klient SimpleX |     -------------- TLS ---------------      | SimpleX        |
|      Chat      |----> SimpleX Messaging Protocol (SMP) ----> | Messaging      |
+----------------+     ----------------------------------      | Server         |
     ^    |             |                              |       +----------------+
     |    |             |                              |            |   |
     |    |             |                              |            | S | T
     |    |             |                              |            | M | L
     |    |             |                              |            | P | S
     |    |             |                              |            |   |
     |    |             |                              |       +----------------+       +----------------+
     |    |            -------------- TLS ---------------      | SimpleX        |       | SimpleX        |
     |    |----------->   Zarządzanie powiadomieniami    ----> | Subskrybent    | ----> | Serwer         |
     |                 ----------------------------------      | powiadomień    |       | Push           |
     |                  |                              |       +----------------+       +----------------+
     |                  |                              |      (w przyszłości może               |
     |                  |                              |        być samodzielnie                |
     |                  |                              |        hostowany)                      V
     |                 -------------- TLS ---------------                              +-----------------+
     |-----------------       Dostarczanie powiadomień   <---------------------------- | Serwer Apple PN |
                       ----------------------------------                              +-----------------+
                        |                              |
```

Daj nam znać, co myślisz o tym projekcie i o tym kompromisie prywatności / użyteczności:

- Dla Ciebie jest to akceptowalny kompromis, jeśli możesz wybrać wyłączenie natychmiastowych powiadomień?
- Czy macie jakieś pomysły, jak można by ulepszyć ten projekt?

Dziękuję!
