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
                        Text("Privacy redefined")
                            .font(.headline)
                        Text("The 1st platform that has no user identifiers – 100% private by design!")
                            .lineLimit(3)
                            .font(.subheadline)
                    }
                }
                .padding(.bottom)

                HStack(alignment: .top) {
                    Text("📭")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("Immune to spam and abuse")
                            .font(.headline)
                        Text("People can connect to you only via the links you share.")
                            .font(.subheadline)
                    }
                }
                .padding(.bottom)

                HStack(alignment: .top) {
                    Text("😎")
                        .font(mediumEmojiFont)
                        .frame(width: 40)
                    VStack(alignment: .leading) {
                        Text("Decentralized")
                            .font(.headline)
                        Text("Based on open-source protocol – anybody can run the servers.")
                            .font(.subheadline)
                    }
                }
                .padding(.bottom)

                Spacer()

                Button {
                    withAnimation {
                        chatModel.onboardingStep = .step2_CreateProfile
                    }
                } label: {
                    HStack {
                        Text("Create your profile")
                            .font(.title2)
                        Image(systemName: "greaterthan")
                    }
                }
                .frame(maxWidth: .infinity)

                Spacer()

                Button {

                } label: {
                    Label("How it works", systemImage: "info.circle")
                        .font(.subheadline)
                }
                .padding(.bottom, 8)
                .frame(maxWidth: .infinity)
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
