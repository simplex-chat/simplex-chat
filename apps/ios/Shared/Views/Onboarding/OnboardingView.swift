//
//  OnboardingStepsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct OnboardingView: View {
    @EnvironmentObject var m: ChatModel
    var onboarding: OnboardingStage

    var body: some View {
        switch onboarding {
        case .step1_SimpleXInfo: SimpleXInfo()
        case .step2_CreateProfile: CreateProfile()
        case .step3a_MakeConnection: MakeConnection()
        case .step3b_ConnectViaLink:
            if let appOpenUrl = m.appOpenUrl {
                ConnectViaLink(appOpenUrl: appOpenUrl)
            } else {
                MakeConnection()
            }
        case .step3c_ConnectToDevelopers: ConnectToDevelopers()
        case .onboardingComplete: EmptyView()
        }
    }
}

enum OnboardingStage {
    case step1_SimpleXInfo
    case step2_CreateProfile
    case step3a_MakeConnection
    case step3b_ConnectViaLink
    case step3c_ConnectToDevelopers
    case onboardingComplete
}

struct OnboardingStepsView_Previews: PreviewProvider {
    static var previews: some View {
        OnboardingView(onboarding: .step1_SimpleXInfo)
    }
}
