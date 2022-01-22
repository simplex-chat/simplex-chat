//
//  SimpleXApp.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

@main
struct SimpleXApp: App {
    private let controller: controller
    init() {
        hs_init(0, nil)
        
        let dataDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.path + "/mobile_v1"
        var cstr = dataDir.cString(using: .utf8)!
        let store = chat_init_store(&cstr)
        let user = String.init(cString: chat_get_user(store))
        print(user)
        if user == "{}" {
            var data = "{ \"displayName\": \"test\", \"fullName\": \"ios test\" }".cString(using: .utf8)!
            chat_create_user(store, &data)
        }
        controller = chat_start(store)
        var cmd = "/help".cString(using: .utf8)!
        print(String.init(cString: chat_send_cmd(controller, &cmd)))
    }
    var body: some Scene {
        WindowGroup {
            ContentView(controller: controller)
        }
    }
}
