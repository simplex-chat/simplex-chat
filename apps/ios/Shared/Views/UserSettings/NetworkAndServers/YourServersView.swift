//
//  YourServersView.swift
//  SimpleX (iOS)
//
//  Created by Efim Poberezkin on 30.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

// TODO Put SMP and XFTP servers in single view, not nested
struct YourServersView: View {
    @EnvironmentObject var theme: AppTheme

    var body: some View {
        VStack {
            List {
                Section {
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .smp)
                            .navigationTitle("SMP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Message servers")
                    }
                    
                    NavigationLink {
                        ProtocolServersView(serverProtocol: .xftp)
                            .navigationTitle("XFTP servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Media & file servers")
                    }
                }
            }
        }
    }
}

#Preview {
    YourServersView()
}
