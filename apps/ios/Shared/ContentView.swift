//
//  ContentView.swift
//  Shared
//
//  Created by Evgeny Poberezkin on 17/01/2022.
//

import SwiftUI

struct ContentView: View {
    @State var messages: [String] = ["Start session:"]
    @State var text: String = ""
    
    func sendMessage() {
    }
    
    var body: some View {
        VStack {
            ScrollView {
                LazyVStack {
                    ForEach(messages, id: \.self) { msg in
                        MessageView(message: msg, sent: false)
                    }
                }
                .padding(10)
            }
            .frame(minWidth: 0,
                   maxWidth: .infinity,
                   minHeight: 0,
                   maxHeight: .infinity,
                   alignment: .topLeading)
            HStack {
                TextField("Message...", text: $text)
                   .textFieldStyle(RoundedBorderTextFieldStyle())
                   .frame(minHeight: CGFloat(30))
                Button(action: sendMessage) {
                    Text("Send")
                }.disabled(text.isEmpty)
            }
            .frame(minHeight: CGFloat(30))
            .padding()
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView(text: "Hello!")
    }
}
