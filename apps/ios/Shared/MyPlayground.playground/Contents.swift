import Foundation

var greeting = "Hello, playground"

let jsonEncoder = JSONEncoder()

//jsonDecoder.decode(Test.self, from: "{\"name\":\"hello\",\"id\":1}".data(using: .utf8)!)


var a = [1, 2, 3]

a.removeAll(where: { $0 == 1} )

print(a)
