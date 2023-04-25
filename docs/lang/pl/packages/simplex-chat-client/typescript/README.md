# SimpleX Chat klient JavaScript

Jest to biblioteka TypeScript, która definiuje klienta WebSocket API dla [Terminal CLI SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/CLI.md), który powinien być uruchomiony jako serwer WebSockets na dowolnym porcie:

```bash
simplex-chat -p 5225
```

Client API zapewnia typy i funkcje do:

- tworzenia i zmiany profilu użytkownika (choć w większości przypadków można to zrobić ręcznie, za pośrednictwem aplikacji terminalowej SimpleX Chat).
- tworzyć i akceptować zaproszenia lub łączyć się z kontaktami.
- tworzyć i zarządzać długoterminowym adresem użytkownika, automatycznie akceptując prośby o połączenie.
- tworzyć, dołączać i zarządzać grupą.
- wysyłać i odbierać pliki.

## Przypadki użycia

- Czatboty: można zaimplementować dowolną logikę łączenia się z użytkownikami SimpleX Chat i komunikowania się z nimi. Za pomocą grup czatowych czatbot może łączyć ze sobą użytkowników SimleX Chat.
- sterowanie urządzeniami: np. serwerami lub automatyką domową. SimpleX Chat zapewnia bezpieczne i autoryzowane połączenia, więc jest to bezpieczniejsze niż korzystanie z rest API.

Proszę podzielić się swoimi przypadkami użycia i implementacjami.

## Szybki start

```
npm i simplex-chat
npm run build
```

Zobacz przykład prostego bota czatowego w [squaring-bot.js](https://github.com/simplex-chat/simplex-chat/blob/stable/packages/simplex-chat-client/typescript/examples/squaring-bot.js):

- uruchom `simplex-chat` jako serwer na porcie 5225: `simplex-chat -p 5225 -d test_db`.
- uruchom chatbota: `node examples/squaring-bot`
- połącz się z chatbotem poprzez klienta SimpleX Chat używając adresu czatbota

## Dokumentacja

Proszę zapoznać się z dostępnym API klienta w [client.ts](https://github.com/simplex-chat/simplex-chat/blob/stable/packages/simplex-chat-client/typescript/src/client.ts).

## Licencja

[AGPL v3](https://github.com/simplex-chat/simplex-chat/blob/stable/packages/simplex-chat-client/typescript/LICENSE)
