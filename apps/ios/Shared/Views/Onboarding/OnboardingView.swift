//
//  OnboardingStepsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/navigation.md

import SwiftUI

// Spec: spec/client/navigation.md#OnboardingView
struct OnboardingView: View {
    var onboarding: OnboardingStage

    var body: some View {
        NavigationView {
            switch onboarding {
            case .step1_SimpleXInfo:
                SimpleXInfo(onboarding: true)
                    .modifier(ThemedBackground())
            case .step2_CreateProfile: // deprecated
                CreateFirstProfile()
                    .modifier(ThemedBackground())
            case .step3_CreateSimpleXAddress: // deprecated
                CreateSimpleXAddress()
            case .step3_ChooseServerOperators: // deprecated - map to new step 3
                YourNetworkView()
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            case .step4_SetNotificationsMode: // deprecated - map to new step 3
                YourNetworkView()
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            case .step3_YourNetwork:
                YourNetworkView()
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            case .step4_NetworkCommitments:
                NetworkCommitmentsView(selectedOperatorIds: Set(ChatModel.shared.conditions.serverOperators.filter { $0.enabled }.map { $0.operatorId }))
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            case .onboardingComplete: EmptyView()
            }
        }
    }
}

func onboardingButtonPlaceholder() -> some View {
    Spacer().frame(height: 40)
}

// Spec: spec/client/navigation.md#onboardingStage
enum OnboardingStage: String, Identifiable {
    case step1_SimpleXInfo
    case step2_CreateProfile // deprecated
    case step3_CreateSimpleXAddress // deprecated
    case step3_ChooseServerOperators // deprecated
    case step4_SetNotificationsMode // deprecated
    case step3_YourNetwork
    case step4_NetworkCommitments
    case onboardingComplete

    public var id: Self { self }
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboarding: .step1_SimpleXInfo)
    }
}
