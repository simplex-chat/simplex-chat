//import UIKit
import SimpleXChat

let s =
"""
{}
"""
//let s = "\"2022-04-24T11:59:23.703162Z\""
let json = getJSONDecoder()
let d = s.data(using: .utf8)!
print (try! json.decode(APIResponse.self, from: d))
