# Prosty przykład czatbota SimpleX

Ten czatbot jest implementacją Haskella dla czatbota REPL.

Wszystko co musisz zrobić, aby stworzyć swojego bota na podstawie tego przykładu, to dostarczyć wiadomość powitalną dla łączących się użytkowników oraz funkcję typu `Contact -> String -> IO String`. Funkcja ta powinna przekształcić wysłaną wiadomość w wiadomość zwrotną, ignorując wszelkie komunikaty systemowe związane z preferencjami i zmianami profilu użytkownika.

Ten przykładowy bot oblicza kwadrat liczby, która została do niego wysłana, ale można go zaprogramować do robienia innych rzeczy, po prostu zmieniając funkcję REPL:

- bardziej zaawansowany kalkulator (np. oparty na [tym](https://github.com/jonathanknowles/haskell-calculator)).
- tłumaczenie na/z dowolnego języka.
- lookup notowań rynkowych.
- wyszukiwanie informacji.
- Dialog napędzany przez AI - bot może utrzymywać dowolny stan rozmowy na podstawie kontaktu.
- dostarczać dowolne inne usługi online poprzez chat UI.
- itp.

Proszę podzielić się z nami wszelkimi botami, które tworzysz, dodamy do tej strony i możemy je hostować, jeśli chcesz!
