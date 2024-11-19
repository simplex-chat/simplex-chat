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
        switch onboarding {
        case .step1_SimpleXInfo: SimpleXInfo(onboarding: true)
        case .step2_CreateProfile: CreateFirstProfile()
        case .step3_CreateSimpleXAddress: CreateSimpleXAddress()
        case .step3_ChooseServerOperators: ChooseServerOperators(onboarding: true)
        case .step4_SetNotificationsMode: SetNotificationsMode()
        case .onboardingComplete: EmptyView()
        }
    }
}

enum OnboardingStage: String, Identifiable {
    case step1_SimpleXInfo
    case step2_CreateProfile // deprecated
    case step3_CreateSimpleXAddress // deprecated
    case step3_ChooseServerOperators
    case step4_SetNotificationsMode
    case onboardingComplete

    public var id: Self { self }
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboarding: .step1_SimpleXInfo)
    }
}
