---
layout: layouts/article.html
title: "Simplex Chat"
date: 2020-10-22
preview: The prototype of SimpleX Messaging Server implementing SMP protocol.
permalink: "/blog/20201022-simplex-chat.html"
---

# Simplex chat

**Opublikowano:** 22 październik, 2020

[https://simplex.chat/pl](https://simplex.chat/pl)

Byłbym bardzo wdzięczny za opinie, krytykę i sugestie dotyczące pomysłu open-source, nad którym powoli pracowałem od początku 2020 roku. Niedawno zrobiłem serwer demonstracyjny dla niskopoziomowego protokołu kolejki wiadomości ("simplex messaging protocol") i stronę internetową, aby spróbować wyjaśnić pomysł czatu, który używałby tego protokołu.

Implementacja protokołu Haskella: [https://github.com/simplex-chat/simplexmq](https://github.com/simplex-chat/simplexmq)

W skrócie, protokół definiuje minimalistyczny zestaw poleceń i odpowiedzi serwera (tylko 7 poleceń i 5 odpowiedzi wysyłanych przez TCP) do obsługi szyfrowanych kolejek wiadomości z utrzymywaniem w pamięci - implementacja używa STM.

Jakby co, to zdecydowanie pomagało w poznaniu typów Haskella itp. znacznie głębiej niż wcześniej :)

Każda krytyka byłaby świetna - z góry dziękuję!

Pierwotnie opublikowane na [https://www.reddit.com/r/haskell/comments/jg6uh4/simplex_chat/](https://www.reddit.com/r/haskell/comments/jg6uh4/simplex_chat/)
