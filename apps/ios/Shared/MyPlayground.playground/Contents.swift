import Foundation

var greeting = "Hello, playground"

let jsonEncoder = JSONEncoder()

//jsonDecoder.decode(Test.self, from: "{\"name\":\"hello\",\"id\":1}".data(using: .utf8)!)


var a = [1, 2, 3]

a.removeAll(where: { $0 == 1} )

print(a)

let input = "This is a test with the привет 🙂 URL https://www.hackingwithswift.com to be detected."
let detector = try! NSDataDetector(types: NSTextCheckingResult.CheckingType.link.rawValue)
let matches = detector.matches(in: input, options: [], range: NSRange(location: 0, length: input.count))

print(matches)

for match in matches {
    guard let range = Range(match.range, in: input) else { continue }
    let url = input[range]
    print(url)
}

let r = try! NSRegularExpression(pattern: "^\\+?[0-9\\.\\(\\)\\-]{7,20}$")

print(r.firstMatch(in: "+44(0)7448-736-790", options: [], range: NSRange(location: 0, length: "+44(0)7448-736-790".count)) == nil)
