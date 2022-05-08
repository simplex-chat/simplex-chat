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
        case .step2_CreateProfile: CreateProfile()
        case .step3_MakeConnection: MakeConnection()
        case .onboardingComplete: EmptyView()
        }
    }
}

enum OnboardingStage {
    case step1_SimpleXInfo
    case step2_CreateProfile
    case step3_MakeConnection
    case onboardingComplete
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboarding: .step1_SimpleXInfo)
    }
}
