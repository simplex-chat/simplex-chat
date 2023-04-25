# Rozwój aplikacji na Androida

Ten readme jest obecnie stubem i jako taki jest w trakcie rozwoju.

Ostatecznie, ten readme będzie działał jako przewodnik do przyczyniania się do rozwoju SimpleX android app.


## Gotchas

#### Podpis SHA do weryfikacji dla linków aplikacji / głębokich linków

Aby aplikacja SimpleX została automatycznie przystosowana do otwierania linków z https://simplex.chat/pl, odcisk palca certyfikatu SHA dla aplikacji zainstalowanej na telefonie musi znajdować się w hostowanym pliku [assetlinks.json](https://simplex.chat/.well-known/assetlinks.json) na simplex.chat.

Akceptowane odciski palców znajdują się na liście `sha256_cert_fingerprints`.

Aby znaleźć odcisk palca swojego certyfikatu SHA wykonaj następujące kroki.

1. Zbuduj i zainstaluj swoją wersję rozwojową aplikacji jak zwykle.
2. Z terminala w Android studio uruchom `adb shell pm get-app-links chat.simplex.app`.
3. Skopiuj podpis wymieniony w `signatures` w wyniku.
4. Dodaj swój podpis do [assetlinks.json](https://github.com/simplex-chat/website/blob/master/.well-known/assetlinks.json) w [website repo](https://github.com/simplex-chat/website) i zrób PR. Po zatwierdzeniu, poczekaj kilka minut na propagację zmian do publicznej strony internetowej, a następnie powinieneś być w stanie zweryfikować SimpleX.

Więcej informacji jest dostępnych [tutaj](https://developer.android.com/training/app-links/verify-site-associations#manual-verification). Jeśli nie ma odpowiedzi po uruchomieniu polecenia `pm get-app-links`, intencje w `AndroidManifest.xml` są prawdopodobnie źle określone. Próba weryfikacji może być wywołana przy użyciu `adb shell pm verify-app-links --re-verify chat.simplex.app`.

Zauważ, że nie jest to problem dla kompilacji aplikacji w sklepie, ponieważ jest to podpisane z naszymi poświadczeniami sklepu z aplikacjami, a zatem istnieje stabilny podpis nad użytkownikami. Programiści nie mają ogólnego dostępu do tych danych uwierzytelniających do rozwoju i testowania.
