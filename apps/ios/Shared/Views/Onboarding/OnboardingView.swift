//
//  OnboardingStepsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct OnboardingView: View {
    var onboardingStep: OnboardingStep

    var body: some View {
        switch onboardingStep {
        case .step1_SimpleXInfo: SimpleXInfo()
        case .step2_CreateProfile: CreateProfile()
        case .step3a_MakeConnection: MakeConnection()
        case .step3b_ConnectViaLink: ConnectViaLink()
        case .step3c_ConnectToDevelopers: ConnectToDevelopers()
        }
    }
}

enum OnboardingStep {
    case step1_SimpleXInfo
    case step2_CreateProfile
    case step3a_MakeConnection
    case step3b_ConnectViaLink
    case step3c_ConnectToDevelopers
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboardingStep: .step1_SimpleXInfo)
    }
}
