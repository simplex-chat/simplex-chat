//
//  MessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 18/01/2022.
//

import SwiftUI

struct MessageView: View {
    var message: String
    var sent: Bool
    let receivedColor: Color = Color(UIColor(red: 240/255, green: 240/255, blue: 240/255, alpha: 1.0))

    var body: some View {
        Text(message)
            .padding(10)
            .foregroundColor(sent ? Color.white : Color.black)
            .background(sent ? Color.blue : receivedColor)
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
        MessageView(message: "> Send message: \"Hello world!\"\nSuccessful", sent: false)
    }
}
