//
//  OnboardingStepsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

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
            case .step3_ChooseServerOperators:
                OnboardingConditionsView()
                    .navigationBarBackButtonHidden(true)
                    .modifier(ThemedBackground())
            case .step4_SetNotificationsMode:
                SetNotificationsMode()
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

enum OnboardingStage: String, Identifiable {
    case step1_SimpleXInfo
    case step2_CreateProfile // deprecated
    case step3_CreateSimpleXAddress // deprecated
    case step3_ChooseServerOperators // changed to simplified conditions
    case step4_SetNotificationsMode
    case onboardingComplete

    public var id: Self { self }
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboarding: .step1_SimpleXInfo)
    }
}
