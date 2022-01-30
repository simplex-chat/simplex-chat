import Foundation

var greeting = "Hello, playground"

let jsonEncoder = JSONEncoder()

let ct = Contact(
    contactId: 123,
    localDisplayName: "ep",
    profile: Profile(displayName: "ep", fullName: "")
)

//let data = try! jsonEncoder.encode(ChatResponse.contactConnected(contact: ct))

//print(String(decoding: data, as: UTF8.self))

//var str = """
//{"resp":{"apiChats":{"chats":
//[{"chatItem":null,"chatInfo":{"direct":{"contact":{"contactId":2,"profile":
//{"displayName":"simplex","fullName":""},"activeConn":
//{"connLevel":0,"entityId":2,"connType":"contact","connId":1
//,"agentConnId":"QTRteFhTR1dWQnpQZHE3NQ==","createdAt":"2022-01-27T19:43:44.015562Z","connStatus":"ready"},"localDisplayName":"simplex"}}}},
//{"chatItem":null,"chatInfo":{"direct":{"contact":{"contactId":3,"profile":
//{"displayName":"ep","fullName":"Evgeny"},"activeConn":
//{"connLevel":0,"entityId":3,"connType":"contact","connId":2
//,"agentConnId":"cTdFNkprSHhZZmZhdWFQVg==","createdAt":"2022-01-27T19:47:08.891646Z","connStatus":"ready"},"localDisplayName":"ep"}}}}]}}}
//"""

//var str = """
//[{"chatItem":null,"chatInfo":{"direct":{"contact":{"contactId":2,"profile":
//{"displayName":"simplex","fullName":""},"activeConn":
//{"connLevel":0,"entityId":2,"connType":"contact","connId":1
//,"agentConnId":"QTRteFhTR1dWQnpQZHE3NQ==","createdAt":"2022-01-27T19:43:44.015562Z","connStatus":"ready"},"localDisplayName":"simplex"}}}},
//{"chatItem":null,"chatInfo":{"direct":{"contact":{"contactId":3,"profile":
//{"displayName":"ep","fullName":"Evgeny"},"activeConn":
//{"connLevel":0,"entityId":3,"connType":"contact","connId":2
//,"agentConnId":"cTdFNkprSHhZZmZhdWFQVg==","createdAt":"2022-01-27T19:47:08.891646Z","connStatus":"ready"},"localDisplayName":"ep"}}}}]
//"""
//

//let str = """
//{"resp":{"apiDirectChat":{"chat":{"chatInfo":{"direct":{"contact":{"contactId":2,"localDisplayName":"ep","profile":{"displayName":"ep","fullName":"Evgeny"},"activeConn":{"connId":1,"agentConnId":"bUk2OXZlN3lfNXFaVWRWMQ==","connLevel":0,"connType":"contact","connStatus":"ready","entityId":2,"createdAt":"2022-01-29T11:21:18.669786Z"}}}},"chatItems":[{"chatDir":{"directSnd":{}},"meta":{"itemId":1,"itemTs":"2022-01-29T11:21:47.947865Z","itemText":"hello","localItemTs":"2022-01-29T11:21:47.947865Z","createdAt":"2022-01-29T11:21:47.947865Z"},"content":{"sndMsgContent":{"msgContent":{"type":"text","text":"hello"}}}},{"chatDir":{"directRcv":{}},"meta":{"itemId":2,"itemTs":"2022-01-29T11:22:08Z","itemText":"hi","localItemTs":"2022-01-29T11:22:08Z","createdAt":"2022-01-29T11:22:08.563959Z"},"content":{"rcvMsgContent":{"msgContent":{"type":"text","text":"hi"}}}}]}}}}
//"""

let str = "\"2022-01-29T11:21:47Z\""

let data = str.data(using: .utf8)!

let jsonDecoder = JSONDecoder()

let df1 = DateFormatter()
df1.locale = Locale(identifier: "en_US_POSIX")
df1.dateFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZZZZZ"
df1.timeZone = TimeZone(secondsFromGMT: 0)

let df2 = DateFormatter()
df2.locale = Locale(identifier: "en_US_POSIX")
df2.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZZZZZ"
df2.timeZone = TimeZone(secondsFromGMT: 0)

jsonDecoder.dateDecodingStrategy = .iso8601 // .custom { decoder in
//    let container = try decoder.singleValueContainer()
//    let string = try container.decode(String.self)
//    if let date = df1.date(from: string) ?? df2.date(from: string) {
//        return date
//    }
//    throw DecodingError.dataCorruptedError(in: container, debugDescription: "Invalid date: \(string)")
//}


let r: Date = try! jsonDecoder.decode(Date.self, from: data)

print(r)

struct Test: Decodable {
    var name: String
    var id: Int64 = 0
}

//jsonDecoder.decode(Test.self, from: "{\"name\":\"hello\",\"id\":1}".data(using: .utf8)!)

"\(ChatType.direct)"
