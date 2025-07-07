//
//  InProgressOverlayView.swift
//  SimpleX
//
//  Created by spaced4ndy on 07.07.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct InProgressOverlayView: View {
    @EnvironmentObject var inProgressManager: InProgressManager

    var body: some View {
        Group {
            if let state = inProgressManager.inProgressState {
                ZStack {
                    Color.black.opacity(0.4)
                        .ignoresSafeArea()

                    Group {
                        if let text = state.text {
                            ProgressView(text)
                        } else {
                            ProgressView()
                        }
                    }
                    .padding()
                    .background(Color(.systemBackground))
                    .cornerRadius(12)
                }
                .transition(.opacity)
                .animation(.easeInOut(duration: 0.2), value: inProgressManager.isLoading)
            }
        }
    }
}

#Preview {
    InProgressOverlayView()
}
