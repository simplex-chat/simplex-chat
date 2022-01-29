//
//  SendMessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SendMessageView: View {
    var sendMessage: (String) -> Void
    var inProgress: Bool = false
    @State var command: String = ""

    var body: some View {
        HStack {
            TextField("Message...", text: $command)
               .textFieldStyle(RoundedBorderTextFieldStyle())
               .textInputAutocapitalization(.never)
               .disableAutocorrection(true)
               .frame(minHeight: 30)
               .onSubmit(submit)

            if (inProgress) {
                ProgressView()
                    .frame(width: 40, height: 20, alignment: .center)
            } else {
                Button(action: submit) {
                    Text("Send")
                }.disabled(command.isEmpty)
            }
        }
        .frame(minHeight: 30)
        .padding()
    }

    func submit() {
        sendMessage(command)
        command = ""
    }
}

struct SendMessageView_Previews: PreviewProvider {
    static var previews: some View {
        SendMessageView(sendMessage: { print ($0) })
    }
}
