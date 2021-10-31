//
//  ContentView.swift
//  SimpleX Chat
//
//  Created by Evgeny on 30/10/2021.
//

import SwiftUI

struct MessageView: View {
    var message: String
    var isCurrentUser: Bool
    
    var body: some View {
        Text(message)
            .padding(10)
            .foregroundColor(isCurrentUser ? Color.white : Color.black)
            .background(isCurrentUser ? Color.blue : Color(UIColor(red: 240/255, green: 240/255, blue: 240/255, alpha: 1.0)))
            .cornerRadius(10)
            .frame(minWidth: 100,
                    maxWidth: .infinity,
                    minHeight: 0,
                    maxHeight: .infinity,
                    alignment: .leading)
    }
}

struct MessageView_Previews: PreviewProvider {
    static var previews: some View {
        MessageView(message: "> Send message: \"Hello world!\"\nSuccessful", isCurrentUser: false)
    }
}
