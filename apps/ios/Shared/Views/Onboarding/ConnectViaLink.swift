//
//  ConnectViaLink.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ConnectViaLink: View {
    @EnvironmentObject var m: ChatModel
    var appOpenUrl: URL

    var body: some View {
        VStack(alignment: .leading) {
            Text("Connect via link")
                .font(.largeTitle)
                .padding(.vertical)

            Text("You received this link:")
                .padding(.bottom)

            Text(appOpenUrl.absoluteString)
                .font(.caption)
                .lineLimit(16)
                .padding(.bottom)

            Button("Tap here to connect") {
                connectViaLink(appOpenUrl.absoluteString)
                m.onboardingStage = .onboardingComplete
            }
            Spacer()
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }
}

struct ConnectViaLink_Previews: PreviewProvider {
    static var previews: some View {
        ConnectViaLink(appOpenUrl: URL(string: "https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")!)
    }
}
