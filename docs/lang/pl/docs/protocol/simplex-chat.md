PROJEKT Zmiana 0.1, 08/08/2022

Evgeny Poberezkin

# SimpleX Chat Protocol

## Abstrakt

SimpleX Chat Protocol to protokół używany przez klientów SimpleX Chat do wymiany wiadomości. Protokół ten opiera się na protokołach SimpleX niższego poziomu - SimpleX Messaging Protocol (SMP) i SimpleX Messaging Agent protocol. SimpleX Chat Protocol opisuje format wiadomości oraz operacje klienta, które powinny być wykonywane podczas odbierania takich wiadomości.

## Zakres

Zakres SimpleX Chat Protocol to komunikaty na poziomie aplikacji, zarówno dla funkcji czatu, związanych z rozmowami między klientami, jak i rozszerzalne dla dowolnych innych funkcji aplikacji. Obecnie obsługiwane funkcje czatu:

- wiadomości bezpośrednie i grupowe,
- odpowiedzi na wiadomości (cytowanie), przekazywanie wiadomości dalej i usuwanie wiadomości,
- załączniki wiadomości: obrazy i pliki,
- tworzenie i zarządzanie grupami czatu,
- zapraszanie i sygnalizacja połączeń audio/wideo WebRTC.

## Ogólny format wiadomości.

Protokół SimpleX Chat obsługuje dwa formaty wiadomości:

- format oparty na JSON dla wiadomości czatu i aplikacji.
- format binarny do przesyłania plików lub dowolnych innych danych binarnych.

### Format JSON dla wiadomości czatu i aplikacji

Ten dokument wykorzystuje schematy JTD [RFC 8927](https://www.rfc-editor.org/rfc/rfc8927.html) do określenia właściwości wiadomości czatu, z pewnymi dodatkowymi ograniczeniami właściwości wiadomości zawartymi w członie metadanych schematów JTD. W przypadku jakiejkolwiek sprzeczności między przykładami JSON a schematem JTD, ten ostatni MUSI być uznany za poprawny.

Białe spacje są używane w przykładach JSON dla czytelności, klienci SimpleX Chat Protocol MUSZĄ unikać używania białych spacji podczas kodowania wiadomości JSON.

Ogólny format wiadomości jest określony przez ten schemat JTD:

```JSON
{
  "properties": {
    "event": {
      "type": "string"
    },
    "msgId": {
      "type": "string",
      "metadata": {
        "format": "base64url-encoded 12 bytes random message ID"
      }
    },
    "params": {
      "optionalProperties": {}
    }
  }
}
```

Na przykład ta wiadomość definiuje prostą wiadomość tekstową `"hello!"`:

```JSON
{
  "event": "x.msg.new",
  "msgId": "abcd",
  "params": {
    "content": {
      "type": "text",
      "text": "hello!"
    }
  }
}
```

Właściwość `msgId` jest 12 bajtowym base64url-encoded losowym ID wiadomości, które klient może użyć do odniesienia się do wiadomości w przyszłości, np. podczas edycji, cytowania lub usuwania.

Właściwość `event` jest typem wiadomości, który definiuje semantykę wiadomości i dozwolony format właściwości `params`.

Właściwość `params` zawiera dane wiadomości, zależne od `event`, zdefiniowane poniżej oraz w [schemacie JTD](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/simplex-chat.schema.json).

### Format binarny do wysyłania plików

Klienci SimpleX Chat używają oddzielnych połączeń do wysyłania plików przy użyciu formatu binarnego. Rozmiar kawałka pliku wysyłanego w każdej wiadomości NIE MOŻE być większy niż 15 780 bajtów, aby zmieścić się w 16kb (16384 bajtów) bloku transportowego.

Składnia każdej wiadomości używanej do wysyłania plików jest zdefiniowana przez następującą notację ABNF:

```abnf
fileMessage = fileChunk / cancelFile
fileChunk = %s"F" chunkNo chunk
cancelFile = %s"C"
chunkNo = 4*4 OCTET ; 32bit word sequential chunk number, starting from 1, in network byte order
chunk = 1*15780 OCTET ; file data, up to 15,780 bytes
```

Pierwszy numer chunk MUSI być 1.

## Wiadomości i elementy czatu

Choć użytkownicy zwykle używają terminu "wiadomość" w odniesieniu do obiektów prezentowanych w rozmowie, oczekiwana funkcjonalność tych obiektów sprawia, że jest to błędny termin. "Wiadomości" mają być niezmienne; nie można ich modyfikować ani usuwać po wysłaniu. Od obiektów w rozmowie oczekuje się, że będą mutowalne. Ten dokument i implementacja używają terminu "element czatu" w odniesieniu do tych obiektów, aby odróżnić je od wiadomości przesyłanych między klientami.

## Obsługiwane typy wiadomości JSON i podprotokoły SimpleX Chat

Typy wiadomości są wysyłane jako ciąg we właściwości `event` wiadomości JSON. Ogólna składnia ciągu zdarzeń jest określona przez ten ABNF:

```abnf
event = namespace "." subprotocol *("." eventWord)
namespace = eventWord ; 1-letter recommended
subprotocol = eventWord
eventWord = 1* ALPHA
```

Wszystkie komunikaty SimpleX Chat Protocol związane z funkcjami czatu są zdefiniowane w przestrzeni nazw `x`.

Podprotokół to grupa wiadomości dla powiązanych funkcji wiadomości - np. wysyłanie plików, zarządzanie grupami czy negocjowanie połączeń WebRTC.

Protokół SimpleX Chat obsługuje następujące typy wiadomości przekazywane we właściwości `event`:

- `x.contact` - profil kontaktu i dodatkowe dane wysyłane jako część żądania kontaktu na adres kontaktu długoterminowego.
- `x.info*` - wiadomości służące do wysyłania, aktualizacji i de-duplikacji profili kontaktów.
- `x.msg.*` - wiadomości do tworzenia, aktualizacji i usuwania elementów czatu z zawartością.
- `x.file.*` - wiadomości do akceptacji i anulowania wysyłania plików (patrz podprotokół plików).
- `x.grp.*` - wiadomości używane do zarządzania grupami i członkami grup (patrz podprotokół grupy).
- `x.call.*` - wiadomości służące do zapraszania do połączeń WebRTC i wysyłania wiadomości sygnalizacyjnych.
- `x.ok` - wiadomość wysyłana podczas handshake'u połączenia.

Schemat JTD definiujący komunikaty dla wszystkich funkcji czatu dostępny jest w [tym pliku](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/simplex-chat.schema.json) - należy zapoznać się z tym dokumentem, aby poznać wszystkie właściwości komunikatu `params`.

## x.contact - wysłanie prośby o połączenie

Wiadomość jest wysyłana jako część żądania połączenia na adres użytkownika długoterminowego. Właściwość `contactReqId` jest używana do identyfikacji zduplikowanego żądania kontaktu - klient odbierający MOŻE umieścić powtórzone żądanie na górze listy w UI.

## Podprotokół dla profilu kontaktowego

### x.info - profil kontaktowy

Wiadomość ta jest wysyłana przez obie strony połączenia podczas handshake'u połączenia i może być wysyłana również później, gdy profil kontaktu jest aktualizowany.

### Szukanie zduplikowanych kontaktów

Ponieważ nie ma globalnie unikalnych identyfikatorów użytkownika, kiedy kontakt, z którym użytkownik jest już połączony, zostanie dodany do grupy przez innego członka grupy, ten kontakt zostanie dodany do listy kontaktów użytkownika jako nowy kontakt. Aby umożliwić łączenie takich kontaktów, "sonda" (losowe 32 bajty zakodowane base64url) POWINNA być wysłana do wszystkich nowych członków jako część wiadomości `x.info.probe`, a w przypadku gdy istnieje kontakt o tym samym profilu, hash sondy MOŻE być wysłany do niego jako część wiadomości `x.info.probe.check`. W przypadku gdy zarówno nowy członek jak i istniejący kontakt są tym samym użytkownikiem (otrzymają zarówno sondę jak i jej hash), kontakt odeśle oryginalną sondę jako część wiadomości `x.info.probe.ok` poprzez poprzednio istniejące połączenie kontaktowe - udowadniając nadawcy, że ten nowy członek i istniejący kontakt są tym samym użytkownikiem, w którym to przypadku nadawca POWINIEN połączyć te dwa kontakty.

Klienci wysyłający mogą wyłączyć tę funkcjonalność, a klienci odbierający mogą ignorować wiadomości sondujące.

Jeśli klient wysyłający używa wiadomości `x.info.probe`, MUSI je wysyłać do wszystkich nowych członków, a nie tylko wtedy, gdy istnieje pasujący profil kontaktu. Ma to na celu uniknięcie wycieku informacji o istnieniu pasującego profilu kontaktowego.

## Podprotokół dla wiadomości o treści

### x.msg.new - nowa wiadomość o treści

Kiedy klienci czatu otrzymują lub wysyłają tę wiadomość, MUSZĄ utworzyć nową pozycję w rozmowie. Właściwość najwyższego poziomu `msgId` jest zdefiniowana, aby umożliwić odniesienie się do tego elementu czatu lub wiadomości w przyszłości, np. w celu usunięcia, uaktualnienia lub zacytowania elementu czatu, lub zaakceptowania pliku.

Ta wiadomość używa właściwości `params` wiadomości jako kontenera treści wiadomości, bez żadnych właściwości najwyższego poziomu dla kontenera. Kontener wiadomości (`params`) zawiera właściwość `content` wiadomości, opcjonalne "zaproszenie" do otrzymania załącznika w postaci pliku lub obrazu we właściwości `file` (która jest interpretowana w zależności od typu zawartości wiadomości) oraz opcjonalne wskazanie czy ta wiadomość jest przekazywana dalej (właściwość kontenera `"forward": true`) lub wysyłana w odpowiedzi na inną wiadomość (właściwość kontenera `"quote": {<quoted message>}`). Zobacz `/definition/msgContainer` w [JTD schema](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/simplex-chat.schema.json) dla formatu kontenera wiadomości.

Treść wiadomości może być jednego z czterech typów:

- `text` - dla tego formatu nie jest oczekiwany załącznik w postaci pliku, właściwość `text` MUSI być niepusta.
- `file` - wymagany jest załączony plik, właściwość `text` MOŻE być pusta.
- `image` - dołączony plik jest wymagany, właściwość `text` MOŻE być pusta.
- `link` - nie oczekuje się załączenia pliku, właściwość `text` MUSI być niepusta. Właściwość `preview` zawiera informacje o podglądzie linku.

Zobacz `/definition/msgContent` w [JTD schema](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/simplex-chat.schema.json) dla formatu kontenera wiadomości.

### x.msg.update - aktualizacja wcześniej wysłanej wiadomości

Ta wiadomość jest używana do aktualizacji wcześniej utworzonego elementu czatu. Jej właściwość `params` zawiera `msgId` poprzednio wysłanej wiadomości, którą ta aktualizuje oraz `content` z treścią wiadomości, której klienci muszą użyć do zastąpienia treści oryginalnego elementu czatu.

Jeśli wiadomość, do której się odwołano nie istnieje, klienci MUSZĄ utworzyć nowy element chatu z ID wiadomości, do której się odwołano. Jeśli komunikat, do którego się odwołano, nie jest komunikatem o treści, klienci MUSZĄ zignorować ten komunikat.

### x.msg.del - żądanie usunięcia poprzednio wysłanego komunikatu

Komunikat ten służy do usuwania wcześniej wysłanych elementów chatu. Klienci odbierający MUSZĄ zaimplementować ją jako soft-delete, zastępując oryginalny element czatu specjalnym elementem wskazującym, że "wiadomość została usunięta", który może być całkowicie usunięty przez użytkownika. Jeśli wiadomość, do której się odwołano nie istnieje lub została wysłana przez innego użytkownika niż ten, który wysłał `x.msg.del`, klienci odbierający MUSZĄ zignorować tę wiadomość. Klientom zaleca się również ograniczenie czasu, w którym usuwanie wiadomości jest dozwolone, zarówno dla nadawców jak i dla odbiorców.

## Podprotokół do wysyłania i odbierania plików

Kiedy treść wiadomości `x.msg.new` zawiera załącznik do pliku (zaproszenie do odbioru pliku), ten podprotokół jest używany do przyjęcia tego pliku lub powiadomienia odbiorcy, że wysłanie pliku zostało anulowane.

Załącznik do pliku może opcjonalnie zawierać adres połączenia do odbioru pliku - klienci MUSZĄ go zawierać przy wysyłaniu plików do bezpośrednich połączeń i NIE MUSZĄ go zawierać przy wysyłaniu załącznika do grupy (ponieważ różni członkowie będą potrzebowali różnych połączeń do odbioru pliku).

Wiadomość `x.file.acpt` jest używana do akceptacji pliku w przypadku, gdy adres połączenia do pliku został zawarty w wiadomości (czyli w przypadku, gdy zaproszenie do pliku zostało wysłane w wiadomości bezpośredniej). Jest wysyłana jako część handshake'u przez połączenie plikowe, dlatego wiadomość ta nie zawiera odniesienia do pliku - używane połączenie zapewnia wystarczający kontekst dla nadawcy.

Wiadomość `x.file.acpt.inv` jest używana do akceptacji pliku w konwersacjach grupowych, zawiera adres połączenia. Jest wysyłana w tym samym połączeniu, w którym plik został zaoferowany i musi odnosić się do oryginalnej wiadomości.

Wiadomość `x.file.cancel` jest wysyłana w celu powiadomienia odbiorcy, że wysłanie pliku zostało anulowane. Jest wysyłana w odpowiedzi na przyjęcie pliku wiadomością `x.file.acpt.inv`. Jest wysyłana w tym samym połączeniu, w którym plik został zaoferowany.

## Podprotokół dla grup czatowych

### Zdecentralizowany projekt dla grup czatowych

Grupy SimpleX Chat są w pełni zdecentralizowane i nie mają żadnych globalnie unikalnych identyfikatorów grupowych - są one definiowane na urządzeniach klienckich jedynie jako profil grupy i zestaw dwukierunkowych połączeń SimpleX z innymi członkami grupy. Kiedy nowy członek przyjmuje zaproszenie do grupy, członek zapraszający wprowadza nowego członka do wszystkich istniejących członków i przekazuje adresy połączeń, aby mogli ustanowić bezpośrednie i grupowe połączenia członków.

Istnieje tutaj możliwość ataku: ponieważ członek wprowadzający przekazuje adresy, może je zastąpić innymi adresami, wykonując atak MITM na komunikację między istniejącymi i wprowadzonymi członkami - jest to podobne do tego, że operator komunikacji może wykonać MITM na każdym połączeniu między użytkownikami. Aby złagodzić ten atak, ten podprotokół grupowy zostanie rozszerzony, aby umożliwić sprawdzanie bezpieczeństwa połączenia poprzez wysyłanie weryfikacji połączenia out-of-band.

ZALECA SIĘ, aby klienci wskazywali w interfejsie użytkownika, czy połączenie z członkiem grupy lub kontaktem zostało nawiązane bezpośrednio czy przez innego użytkownika.

Każdy członek grupy jest identyfikowany przez unikalny dla całej grupy identyfikator używany przez wszystkich członków grupy. Ma to na celu umożliwienie odwoływania się do członków w wiadomościach oraz umożliwienie sprawdzania integralności wiadomości grupowych.

Poniższy diagram przedstawia sekwencję komunikatów wysyłanych pomiędzy klientami użytkowników w celu dodania nowego członka do grupy.

![Dodawanie członka do grupy](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/diagrams/group.svg)

### Role członków

Obecnie członkowie mogą mieć jedną z trzech ról - `owner`, `admin` i `member`. Użytkownik, który stworzył grupę ma samodzielnie przypisaną rolę właściciela, nowi członkowie mają przypisaną rolę przez członka, który ich dodaje - tylko członkowie `owner` i `admin` mogą dodawać nowych członków; tylko członkowie `owner` mogą dodawać członków z rolą `owner`.

### Wiadomości do zarządzania grupami i dodawania członków

Wiadomość `x.grp.inv` jest wysyłana w celu zaproszenia kontaktu do grupy poprzez bezpośrednie połączenie kontaktu i zawiera adres połączenia członka grupy. Ta wiadomość MUSI być wysyłana tylko przez członków z rolą `admin` lub `owner`. Opcjonalny `groupLinkId` jest dołączany gdy ta wiadomość jest wysyłana do kontaktów połączonych poprzez link grupowy użytkownika. Ten identyfikator jest losową sekwencją bajtów, bez globalnej czy nawet lokalnej unikalności - jest on używany tylko w zaproszeniach użytkownika do danej grupy, aby zapewnić potwierdzenie dla kontaktu, że zaproszenie grupowe dotyczy tej samej grupy, z którą kontakt łączył się poprzez link grupowy, tak aby zaproszenie mogło być automatycznie zaakceptowane przez kontakt - kontakt porównuje go z id linku grupowego zawartym w polu danych linku grupowego uri.

Wiadomość `x.grp.acpt` jest wysyłana jako część handshake'u połączenia z członkiem grupy, tylko do użytkownika zapraszającego.

Wiadomość `x.grp.mem.new` jest wysyłana przez użytkownika zapraszającego do wszystkich połączonych członków (i zaplanowana jako oczekująca do wszystkich ogłoszonych, ale jeszcze nie połączonych członków), aby ogłosić nowego członka istniejącym członkom. Ta wiadomość MUSI być wysyłana tylko przez członków z rolą `admin` lub `owner`. Klienci odbierający MUSZĄ zignorować tą wiadomość jeśli jest ona otrzymana od członka z rolą `member`.

Wiadomości `x.grp.mem.intro` są wysyłane przez zapraszającego użytkownika do zaproszonego członka, poprzez jego połączenie z członkiem grupy, jedna wiadomość dla każdego istniejącego członka. Jeśli ta wiadomość jest wysłana przez innego członka niż ten, który zaprosił odbiorcę, MUSI być zignorowana.

Wiadomości `x.grp.mem.inv` są wysyłane przez zaproszonego użytkownika do zapraszającego użytkownika, jedna wiadomość dla każdego istniejącego członka wcześniej przedstawionego za pomocą wiadomości `x.grp.mem.intro`. Kiedy ta wiadomość jest wysłana przez innego członka niż ten, który został zaproszony przez odbiorcę, MUSI być zignorowana.

Wiadomość `x.grp.mem.fwd` jest używana przez zapraszającego użytkownika do przekazania zaproszeń otrzymanych od zaproszonego członka w wiadomości `x.grp.mem.inv` do wszystkich innych członków. Ta wiadomość może być wysłana tylko przez członka, który wcześniej ogłosił nowego członka, w przeciwnym razie odbiorcy MUSZĄ ją zignorować.

`x.grp.mem.info` - wiadomość ta jest wysyłana jako część handshake'u połączenia z członkiem - zawiera profil członka grupy.

Wiadomość `x.grp.mem.role` jest wysyłana w celu aktualizacji roli członka grupy - jest wysyłana do wszystkich członków przez członka, który zaktualizował rolę członka, do którego odnosi się ta wiadomość. Ta wiadomość MUSI być wysłana tylko przez członków z rolą `admin` lub `owner`. Klienci odbierający MUSZĄ zignorować tą wiadomość jeśli otrzymają ją od członka z rolą mniejszą niż `admin`.

Wiadomość `x.grp.mem.del` jest wysyłana w celu usunięcia członka - jest wysyłana do wszystkich członków przez członka, który usuwa członka wymienionego w tej wiadomości. Ta wiadomość MUSI być wysłana tylko przez członków z rolą `admin` lub `owner`. Klienci odbierający MUSZĄ zignorować tą wiadomość jeśli jest ona otrzymana od członka z rolą `member`.

Wiadomość `x.grp.leave` jest wysyłana do wszystkich członków przez członka opuszczającego grupę. Jeśli jedyny właściciel grupy `owner` opuści grupę, nie będzie możliwe usunięcie jej za pomocą wiadomości `x.grp.del` - ale wszyscy członkowie mogą nadal opuścić grupę za pomocą wiadomości `x.grp.leave` i następnie usunąć lokalną kopię grupy.

Wiadomość `x.grp.del` jest wysyłana do wszystkich członków przez członka, który usuwa grupę. Klienci, którzy otrzymali tę wiadomość POWINNI zachować lokalną kopię usuniętej grupy, dopóki nie zostanie ona usunięta przez użytkownika. Ta wiadomość MUSI być wysyłana tylko przez członków z rolą `owner`. Klienci odbierający MUSZĄ zignorować tą wiadomość, jeśli jest ona otrzymana od członka innego niż z rolą `owner`.

Wiadomość `x.grp.info` jest wysyłana do wszystkich członków przez członka, który zaktualizował profil grupy. Tylko właściciele grup mogą aktualizować profile grupowe. Klienci MOGĄ zaimplementować jakąś strategię rozwiązywania konfliktów - obecnie nie jest ona zaimplementowana przez klienta SimpleX Chat. Ta wiadomość MUSI być wysyłana tylko przez członków z rolą `owner`. Klienci odbierający MUSZĄ zignorować tę wiadomość, jeśli jest ona otrzymana od członka innego niż z rolą `owner`.

## Podprotokół dla połączeń audio/wideo WebRTC

Ten podprotokół jest używany do wysyłania zaproszeń do połączeń oraz do negocjowania kluczy szyfrowania end-to-end i przekazywania informacji sygnalizacyjnych WebRTC.

Te wiadomości są wykorzystywane w połączeniach WebRTC:

1. `x.call.inv`: klient inicjujący połączenie wysyła wiadomość `x.call.inv` w bezpośrednim połączeniu, aby zaprosić innego klienta do połączenia. W tym momencie sesja WebRTC nie jest jeszcze zainicjowana, wiadomość ta zawiera tylko typ połączenia i klucz DH do uzgodnienia klucza.

2. `x.call.offer`: aby zaakceptować połączenie, klient odbierający wysyła wiadomość `x.call.offer`. Ta wiadomość zawiera ofertę WebRTC i zebranych kandydatów ICE. Dodatkowi kandydaci ICE mogą być wysłani w wiadomości `x.call.extra`.

3. `x.call.answer`: aby kontynuować połączenie klient inicjujący musi odpowiedzieć wiadomością `x.call.answer`. Wiadomość ta zawiera odpowiedź WebRTC i zebranych kandydatów ICE. Dodatkowi kandydaci ICE mogą być wysłani w wiadomości `x.call.extra`.

4. Wiadomość `x.call.end` jest wysyłana w celu powiadomienia drugiej strony, że połączenie jest zakończone.
