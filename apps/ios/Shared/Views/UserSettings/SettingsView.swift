//
//  SettingsView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SettingsView: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        UserProfile()
        UserAddress()
    }
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = sampleUser
        return SettingsView()
            .environmentObject(chatModel)
    }
}
