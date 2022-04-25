import UIKit

let s = """
          {
            "contactConnection" : {
              "contactConnection" : {
                "viaContactUri" : false,
                "pccConnId" : 456,
                "pccAgentConnId" : "cTdjbmR4ZzVzSmhEZHdzMQ==",
                "pccConnStatus" : "new",
                "updatedAt" : "2022-04-24T11:59:23.703162Z",
                "createdAt" : "2022-04-24T11:59:23.703162Z"
              }
            }
          }
"""
//let s = "\"2022-04-24T11:59:23.703162Z\""
let json = getJSONDecoder()
let d = s.data(using: .utf8)!
print (try! json.decode(ChatInfo.self, from: d))
