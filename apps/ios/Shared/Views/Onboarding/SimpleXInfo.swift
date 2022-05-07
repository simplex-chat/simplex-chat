//
//  SimpleXInfo.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SimpleXInfo: View {
    @EnvironmentObject var chatModel: ChatModel

    var body: some View {
        GeometryReader { g in
            VStack(alignment: .leading) {
                Image("logo")
                    .resizable()
                    .aspectRatio(contentMode: .fit)
                    .frame(width: g.size.width * 0.7)
                    .padding(.bottom)
                Text("The next generation of private messaging")
                    .font(.title)
                    .padding(.bottom)

                HStack(alignment: .top) {
                    Text("🎭")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("World's 1st platform without IDs")
                            .font(.headline)
                        Text("No phones or any other user identifiers – 100% private by design!")
                            .font(.subheadline)
                            .padding(.bottom, 8)
                    }
                }

                HStack(alignment: .top) {
                    Text("😎")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("Immune to spam and abuse")
                            .font(.headline)
                        Text("People can connect to you only via your one-time or long-term link you share.")
                            .font(.subheadline)
                            .padding(.bottom, 8)
                    }
                }

                HStack(alignment: .top) {
                    Text("📭")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("Open and decentralized")
                            .font(.headline)
                        Text("Your profile, contacts and delivered messages are stored only on your device.")
                            .font(.subheadline)
                            .padding(.bottom, 8)
                    }
                }

                HStack(alignment: .top) {
                    Text("🔐")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("More secure")
                            .font(.headline)
                        Text("2-layer end-to-end encryption for the privacy of messages and meta-data")
                            .font(.subheadline)
                    }
                }

                Spacer()

                Button {
                    chatModel.onboardingStep = .step2_CreateProfile
                } label: {
                    HStack {
                        Text("Create your profile")
                            .font(.title2)
                        Image(systemName: "greaterthan")
                    }
                }
                .frame(maxWidth: .infinity)

                Spacer()
            }
        }
        .padding()
    }
}

struct SimpleXInfo_Previews: PreviewProvider {
    static var previews: some View {
        SimpleXInfo()
    }
}
