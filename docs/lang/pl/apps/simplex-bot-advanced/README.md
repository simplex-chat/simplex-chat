# Przykład zaawansowanego bota SimpleX Chat

W większości przypadków [prosty bot REPL](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/apps/simplex-bot/) jest wystarczający, ale w przypadkach, gdy chcesz zaprogramować bardziej zaawansowane scenariusze komunikacyjne, możesz zastosować bardziej złożone podejście oparte na zdarzeniach, jak w tym przykładzie.

Podejście oparte na zdarzeniach pozwala Ci:

- decydować o połączeniu z użytkownikiem lub nie w zależności od dowolnych czynników, np. nazwy wyświetlanej przez użytkownika.
- rozłączać się z użytkownikami, którzy wysyłają zbyt wiele wiadomości lub wysyłają wiadomości, które bot uzna za nieodpowiednie.
- reagować na usuwanie i edytowanie wiadomości.
- przetwarzać odpowiedzi na wiadomości w inny sposób, biorąc pod uwagę oryginalną wiadomość.
- przetwarzać i wysyłać obrazy i wiadomości głosowe.
- tworzyć grupy użytkowników, np. łączyć 2 użytkowników.
- itp.
