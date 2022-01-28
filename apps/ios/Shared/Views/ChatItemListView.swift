//
//  ChatItemListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatItemListView: View {
    @EnvironmentObject var chatModel: ChatModel
    var chatItems: [ChatItem]
    var body: some View {        
        ScrollView {
            LazyVStack {
                ForEach(chatItems, id: \.self) { ci in
                    Text(ci.text())
                }
            }
        }
    }
}

//struct ChatItemListView_Previews: PreviewProvider {
//    static var previews: some View {
//        ChatItemListView()
//    }
//}
