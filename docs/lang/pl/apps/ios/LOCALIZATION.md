# Lokalizacja

## Tworzenie kluczy lokalizacyjnych

Istnieją trzy sposoby, na jakie XCode generuje klucze lokalizacyjne z ciągów znaków:

1. Automatycznie, z tekstów używanych w standardowych komponentach `Text`, `Label`, `Button` itp.

2. Wszystkie ciągi przekazane do zmiennych widoku i parametrów funkcji zadeklarowanych jako typ `LocalizedStringKey`. Do tych parametrów można przekazywać tylko stałe łańcuchowe (ewentualnie, z interpolacją) lub inne zmienne typu `LocalizedStringKey`. Zobacz na przykład ContentView.swift.

3. Wszystkie ciągi znaków opakowane w `NSLocalizedString`. Należy pamiętać, że takie ciągi nie obsługują interpolacji swift, zamiast tego należy używać sformatowanych ciągów:


```swift
String.localizedStringWithFormat(NSLocalizedString("You can now send messages to %@", comment: "notification body"), value)
```

## Dodawanie ciągów znaków do istniejących lokalizacji

1. Wybierz `Product -> Export Localizations...` w menu, wybierz folder `ios` jako miejsce docelowe i `SimpleX Localizations` jako nazwę folderu, potwierdź, aby nadpisać (upewnij się, że nie zapisujesz do podfolderu).
2. Dodaj klucze `target` do lokalizacji, które zostały dodane lub zmienione.
3. Wybierz `Product -> Import Localizations...` dla wszystkich nieangielskich folderów - to zaktualizuje pliki Localizable.

Wartości plików lokalizacyjnych mogą być edytowane bezpośrednio, zmiany zostaną uwzględnione w następnym eksporcie. Postępowanie zgodnie z powyższym procesem gwarantuje jednak, że wszystkie ciągi znaków zostaną zlokalizowane.

## Rozwój

Upewnij się, że włączyłeś opcję `Show non-localized strings` w menu `Product -> Scheme -> Edit scheme...` - będzie ona pokazywać wszystkie nielokalne ciągi znaków jako wszystkie kapitalikami.

Przeczytaj więcej o edycji plików XLIFF i łańcuchów tutaj: https://developer.apple.com/documentation/xcode/editing-xliff-and-strings-files
