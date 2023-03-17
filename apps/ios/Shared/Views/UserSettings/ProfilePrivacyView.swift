//
//  ProfilePrivacyView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 17/03/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ProfilePrivacyView: View {
    @EnvironmentObject var m: ChatModel
    @State private var currProtection = ProfileProtection(user: ChatModel.shared.currentUser!)
    @State private var protection = ProfileProtection(user: ChatModel.shared.currentUser!)

    var body: some View {
        let user = m.currentUser!
        List {
            Section() {
                ProfilePreview(profileOf: user)
                    .padding(.leading, -8)
            }

            Section {
                Toggle("Show notifications", isOn: $protection.showNtfs)
                ProfilePasswordView(
                    toggleLabel: "Hide profile",
                    pwdLabel: "Password to show",
                    protect: $protection.hide
                )
            } header: {
                Text("Hidden profile")
            } footer: {
                if protection.hide.enable {
                    Text("To reveal your hidden profile, enter a full password into a search field in **Your chat profiles** page.")
                }
            }

            if protection.hide.enable {
                Section {
                    ProfilePasswordView(
                        toggleLabel: "Wipe via search",
                        pwdLabel: "Password to wipe",
                        protect: $protection.wipe
                    )
                } header: {
                    Text("Wipe profile")
                } footer: {
                    if protection.wipe.enable {
                        Text("To delete the profile **without any confirmation**, enter a full wipe password.")
                    }
                }
            }
        }
    }
}

private struct ProfileProtection {
    var showNtfs: Bool
    var hide: Protection
    var wipe: Protection

    init() {
        showNtfs = true
        hide = Protection()
        wipe = Protection()
    }

    init(user: User) {
        showNtfs = !(user.hideNtfs ?? false)
        hide = Protection(pwd: user.viewPwdHash)
        wipe = Protection(pwd: user.wipePwdHash)
    }
}

private struct Protection {
    var enable: Bool
    var password: String

    init() {
        enable = false
        password = ""
    }

    init(pwd: UserPwdHash?) {
        enable = pwd != nil
        password = ""
    }
}

private struct ProfilePasswordView: View {
    var toggleLabel: LocalizedStringKey
    var pwdLabel: LocalizedStringKey
    @Binding var protect: Protection

    var body: some View {
        Toggle(toggleLabel, isOn: $protect.enable)
        if protect.enable {
            HStack {
                Text(pwdLabel)
                TextEditor(text: $protect.password)
                    .lineLimit(1)
                    .frame(height: 36)
            }
        }
    }
}


struct ProfilePrivacyView_Previews: PreviewProvider {
    static var previews: some View {
        ProfilePrivacyView()
    }
}
