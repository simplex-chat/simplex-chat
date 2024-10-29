//
//  OperatorView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.10.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

import SwiftUI
import SimpleXChat

struct OperatorView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    let serverProtocol: ServerProtocol
    @Binding var serverOperator: ServerOperator // Binding
    
    var servers: [ServerCfg] // State / Binding

    var proto: String { serverProtocol.rawValue.uppercased() }

    var body: some View {
        return VStack {
            List {
                Section(header: Text("Operator").foregroundColor(theme.colors.secondary)) {
                    Text(serverOperator.name)
                    infoViewLink()
                }

                useOperatorSection()
            }
        }
    }

    private func infoViewLink() -> some View {
        NavigationLink() {
            OperatorInfoView(serverOperator: serverOperator)
                .navigationBarTitle("Operator information")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            Text("Information")
        }
    }

    private func useOperatorSection() -> some View {
        Section(header: Text("Use operator").foregroundColor(theme.colors.secondary)) {
            conditionsViewLink()
            if let reviewDeadline = serverOperator.latestConditionsAcceptance.reviewDeadline {
                infoRow("Review until", deadlineTimestamp(reviewDeadline))
            }
            Toggle("Use operator", isOn: $serverOperator.enabled)
                .disabled(!serverOperator.latestConditionsAcceptance.usageAllowed)
                .foregroundColor(!serverOperator.latestConditionsAcceptance.usageAllowed ? theme.colors.secondary : theme.colors.onBackground)
            Group {
                Toggle("for storage", isOn: $serverOperator.roles.storage)
                Toggle("as proxy", isOn: $serverOperator.roles.proxy)
            }
            .padding(.leading, 24)
            .disabled(!serverOperator.enabled)
            .foregroundColor(!serverOperator.enabled ? theme.colors.secondary : theme.colors.onBackground)
        }
    }

    private func deadlineTimestamp(_ date: Date) -> String {
        let localDateFormatter = DateFormatter()
        localDateFormatter.dateStyle = .medium
        localDateFormatter.timeStyle = .none
        return localDateFormatter.string(from: date)
    }

    @ViewBuilder private func conditionsViewLink() -> some View {
        NavigationLink() {
            UsageConditionsView(serverOperator: $serverOperator)
                .navigationBarTitle("Conditions of use")
                .modifier(ThemedBackground(grouped: true))
                .navigationBarTitleDisplayMode(.large)
        } label: {
            if case .accepted = serverOperator.latestConditionsAcceptance {
                Text("Conditions accepted")
            } else {
                Text("Review conditions")
            }
        }
    }
}

struct OperatorInfoView: View {
    @EnvironmentObject var theme: AppTheme
    var serverOperator: ServerOperator

    var body: some View {
        return VStack {
            List {
                Section(header: Text("Description").foregroundColor(theme.colors.secondary)) {
                    Text(serverOperator.info.description)
                }
                Section(header: Text("Website").foregroundColor(theme.colors.secondary)) {
                    Link("\(serverOperator.info.website)", destination: URL(string: serverOperator.info.website)!)
                }
            }
        }
    }
}

struct UsageConditionsView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @Binding var serverOperator: ServerOperator

    let conditionsText = """
    Lorem ipsum odor amet, consectetuer adipiscing elit. Blandit mauris massa tempor ac; maximus accumsan magnis. Sollicitudin maximus tempor luctus sociosqu turpis dictum per imperdiet porttitor. Efficitur mattis fusce curae id efficitur. Non bibendum elementum faucibus vehicula morbi pulvinar. Accumsan habitant tincidunt sollicitudin taciti ad urna potenti velit. Primis laoreet pharetra magnis est dolor proin viverra.

    Laoreet auctor morbi a varius rutrum diam porta? In ad erat condimentum erat leo ornare. Eu venenatis inceptos rhoncus urna fringilla dis proin ante. Cras dignissim rutrum et faucibus feugiat neque curae tempus. Tellus ligula id dapibus, diam sollicitudin velit odio aliquam lectus. Maecenas ullamcorper arcu interdum cubilia donec iaculis. Maximus penatibus turpis a; vel fermentum ridiculus magna phasellus pellentesque. Eros tellus libero varius potenti; lobortis iaculis.

    Mollis condimentum potenti velit at rutrum tellus maximus suscipit nec. Vehicula aenean dui netus enim aliquam. Aliquam libero rhoncus per pharetra accumsan eros. Urna non eu sem varius vivamus mus tellus aptent quam. Tristique mi natoque lectus volutpat facilisi commodo ac consequat. Proin parturient facilisi senectus egestas ultrices. Fringilla nisi urna convallis molestie lorem varius phasellus a ornare. Ullamcorper varius praesent facilisi habitasse massa.

    Potenti dolor ridiculus est faucibus leo. Euismod consequat ultricies fringilla sociosqu duis sollicitudin. Eget convallis lacinia lacus justo per habitasse parturient. Donec nunc himenaeos pretium donec cursus pharetra ac phasellus? Fringilla sodales egestas orci ligula per ligula semper pellentesque. Potenti non dignissim tempor; orci rutrum elit.

    Habitasse eu sapien eleifend gravida tortor potenti senectus euismod. Lectus enim fames turpis lectus facilisi efficitur elit porttitor facilisi. Nisl quam senectus quam augue integer leo. In aliquam tempor nibh proin felis tortor elementum sodales lacinia. Ut per placerat bibendum magna dapibus fermentum bibendum amet congue. Curae bibendum enim platea per faucibus imperdiet morbi hac varius. Conubia feugiat justo hac faucibus dis.
    """

    var body: some View {
        return VStack {
            List {
                Section {
                    Text(conditionsText)
                }

                Section {
                    if case .accepted = serverOperator.latestConditionsAcceptance {
                        Text("Conditions accepted")
                    } else {
                        Button {
                            // Should call api to save state here, not when saving all servers
                            // (It's counterintuitive to lose to closed sheet or Reset)
                            serverOperator.latestConditionsAcceptance = .accepted
                            dismiss()
                        } label: {
                            Text("Accept conditions")
                        }
                    }
                }
            }
        }
    }
}

#Preview {
    OperatorView(
        serverProtocol: .smp,
        serverOperator: Binding.constant(ServerOperator.sampleData1),
        servers: [ServerCfg.sampleData.preset]
    )
}
