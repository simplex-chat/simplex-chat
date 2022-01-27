import Foundation

var greeting = "Hello, playground"

let jsonEncoder = JSONEncoder()

let ct = Contact(
    contactId: 123,
    localDisplayName: "ep",
    profile: Profile(displayName: "ep", fullName: "")
)

let data = try! jsonEncoder.encode(ChatResponse.contactConnected)

print(String(decoding: data, as: UTF8.self))
