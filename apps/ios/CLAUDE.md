@README.md

## Swift code

- Functions and computed properties are declared in the body of the type they belong to, never at file scope or in an extension of a type declared in this project.
- A member of a SimpleXChat type is declared in that type when everything it uses is available in SimpleXChat. A member that depends on the app is a static member of the app type that uses it; when several app types use it, of the type that names its concept.
- An extension is written only where Swift has no other form:
  - a SwiftUI environment value, which SwiftUI requires as a property of EnvironmentValues;
  - a conformance, required by an API, of a type declared in another module.
- A view modifier is a ViewModifier type applied with `.modifier(_:)`.
- A custom alignment is a static constant of a non-generic type that uses it.
- A new member is declared in its type even when the file already extends that type. Existing extensions and file-scope functions are moved only in a task that concerns them.
