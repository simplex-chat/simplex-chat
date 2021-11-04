//
//  ContentView.swift
//  SimpleX Chat
//
//  Created by Evgeny on 30/10/2021.
//

import SwiftUI

struct ContentView: View {
    @State var history: [String] = ["Start session:"]
    @State var text: String = ""
    @State var inProgress: Bool = false
    
    func sendMessage() {
        DispatchQueue.global().async {
            let string: String = self.$text.wrappedValue
            inProgress = true
            text = ""
            if let result = executeCommand(string),
                let stringResult = String(utf8String: result) {
                sleep(1) // emulate work
                history += [stringResult]
            }
            inProgress = false
        }
    }

    var body: some View {
        VStack {
            ScrollView {
                LazyVStack {
                    ForEach(history, id: \.self) { msg in
                        MessageView(message: msg, isCurrentUser: false)
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
               if (inProgress) {
                   ProgressView()
                       .frame(width: 40,
                              height: 20,
                              alignment: .center)
               }
               else {
                   Button(action: sendMessage) {
                       Text("Send")
                   }.disabled(text.isEmpty)
               }
            }.frame(minHeight: CGFloat(30)).padding()
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}

