@README.md

## Swift code

- An extension is written only where Swift has no other form:
  - a SwiftUI environment value, which SwiftUI requires as a property of EnvironmentValues;
  - a conformance, required by an API, of a type declared in another module.
- Members of a type declared in this project are declared in the body of that type, not in an extension.
- A member of a SimpleXChat type is declared in that type; a value it needs from the app is passed as a parameter.
- A view modifier is a ViewModifier type applied with `.modifier(_:)`.
- A custom alignment is a static constant of a non-generic type that uses it.
- Functions and constants at file scope are kept at file scope.
- A new member is declared in its type even when the file already extends that type. Existing extensions are moved only in a task that concerns them.
