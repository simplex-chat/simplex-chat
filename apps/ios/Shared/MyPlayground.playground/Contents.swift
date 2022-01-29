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
//let data = str.data(using: .utf8)!

let jsonDecoder = JSONDecoder()

//let r: [ChatPreview] = try! jsonDecoder.decode([ChatPreview].self, from: data)
//
//print(r)

struct Test: Decodable {
    var name: String
    var id: Int64 = 0
}

jsonDecoder.decode(Test.self, from: "{\"name\":\"hello\",\"id\":1}".data(using: .utf8)!)

