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
    @State var user: User
    @EnvironmentObject private var m: ChatModel
    @State private var currProtection = ProfileProtection(user: ChatModel.shared.currentUser!)
    @State private var protection = ProfileProtection(user: ChatModel.shared.currentUser!)

    var body: some View {
        List {
            Section() {
                ProfilePreview(profileOf: user)
                    .padding(.leading, -8)
            }

            Section("Profile privacy") {
                settingsRow(protection.showNtfs ? "bolt.fill" : "bolt") {
                    Toggle("Show notifications", isOn: $protection.showNtfs)
                }
                settingsRow(protection.forceIncognito ? "theatermasks.fill" : "theatermasks") {
                    Toggle("Force Incognito", isOn: $protection.forceIncognito)
                }
            }

            Section {
                ProfilePasswordView(
                    toggleLabel: "Hide profile",
                    pwdLabel: "Password to show",
                    currProtect: currProtection.hide,
                    protect: $protection.hide
                )
                if currProtection.hide.enable {
                    settingsRow("lock.rotation") {
                        Button("Update profile password") {
        //                        alert = currentKey == ""
        //                        ? (useKeychain ? .encryptDatabaseSaved : .encryptDatabase)
        //                        : (useKeychain ? .changeDatabaseKeySaved : .changeDatabaseKey)
                        }
                    }
                    .disabled(protection.hide.saveDisabled)
                }
            } header: {
                Text("Hidden profile")
            } footer: {
                if protection.hide.enable {
                    Text("To reveal your hidden profile, enter a full password into a search field in **Your chat profiles** page.")
                }
            }

//            if protection.hide.enable {
//                Section {
//                    ProfilePasswordView(
//                        toggleLabel: "Wipe via search",
//                        pwdLabel: "Password to wipe",
//                        protect: $protection.wipe
//                    )
//                } header: {
//                    Text("Wipe profile")
//                } footer: {
//                    if protection.wipe.enable {
//                        Text("To delete the profile **without any confirmation**, enter a full wipe password.")
//                    }
//                }
//            }
        }
    }
}

private struct ProfileProtection {
    var showNtfs: Bool
    var forceIncognito: Bool
    var hide: Protection
    var wipe: Protection

    init() {
        showNtfs = true
        forceIncognito = true
        hide = Protection()
        wipe = Protection()
    }

    init(user: User) {
        showNtfs = user.showNtfs
        forceIncognito = user.forceIncognito ?? false
        hide = Protection(pwd: user.viewPwdHash)
        wipe = Protection(pwd: user.wipePwdHash)
    }
}

private struct Protection {
    var enable: Bool
    var password: String
    var confirmPassword: String

    init() {
        enable = false
        password = ""
        confirmPassword = ""
    }

    init(pwd: UserPwdHash?) {
        enable = pwd != nil
        password = ""
        confirmPassword = ""
    }

    var confirmValid: Bool { confirmPassword == "" || password == confirmPassword }

    var saveDisabled: Bool { password == "" || confirmPassword == "" || !confirmValid }
}

private struct ProfilePasswordView: View {
    var toggleLabel: LocalizedStringKey
    var pwdLabel: LocalizedStringKey
    var currProtect: Protection?
    @Binding var protect: Protection
    @State private var currPassword: String = ""

    var body: some View {
        Toggle(toggleLabel, isOn: $protect.enable)
        if protect.enable {
            if currProtect?.enable == true {
                PassphraseField(key: $currPassword, placeholder: "Current password", valid: true)
            }
            PassphraseField(key: $protect.password, placeholder: pwdLabel, valid: true, showStrength: true)
            PassphraseField(key: $protect.confirmPassword, placeholder: "Confirm password", valid: protect.confirmValid)
        }
    }
}


struct ProfilePrivacyView_Previews: PreviewProvider {
    static var previews: some View {
        ProfilePrivacyView(user: User.sampleData)
    }
}
