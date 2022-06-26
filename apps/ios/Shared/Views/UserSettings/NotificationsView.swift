//
//  NotificationsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct NotificationsView: View {
    @EnvironmentObject var m: ChatModel
    @State private var notificationMode: NotificationMode?
    @State private var alert: NotificationMode?

    var body: some View {
        List {
            Section {
                NavigationLink {
                    List {
                        Section {
                            SelectionListView(list: NotificationMode.values, selection: $notificationMode) { mode in
                                alert = mode
                            }
                        } footer: {
                            VStack(alignment: .leading) {
                                if let mode = notificationMode {
                                    Text(ntfModeDescription(mode))
                                }
                            }
                            .font(.callout)
                            .padding(.top, 1)
                        }
                    }
                    .navigationTitle("Send notifications")
                    .navigationBarTitleDisplayMode(.inline)
                    .alert(item: $alert) { notificationAlert($0) }
                    .onAppear { notificationMode = m.notificationMode }
                } label: {
                    HStack {
                        Text("Send notifications")
                        Spacer()
                        Text(m.notificationMode.label)
                    }
                }

                NavigationLink {
                    List {
                        Section {
                            SelectionListView(list: NotificationPreviewMode.values, selection: $m.notificationPreview)
                        } footer: {

                        }
                    }
                    .navigationTitle("Show preview")
                    .navigationBarTitleDisplayMode(.inline)
                } label: {
                    HStack {
                        Text("Show preview")
                        Spacer()
                        Text(m.notificationPreview?.label ?? "")
                    }
                }
            } header: {
                Text("Message notifications")
            }
        }
    }

    private func notificationAlert(_ mode: NotificationMode) -> Alert {
        switch mode {
        case .off:
            return Alert(
                title: Text("Turn off notifications?"),
                message: Text(ntfModeDescription(mode)),
                primaryButton: .default(Text("Turn off")) {
                    notificationMode = mode
                    m.notificationMode = mode
                },
                secondaryButton: .cancel() {
                    notificationMode = m.notificationMode
                }
            )
        case .periodic:
            return Alert(
                title: Text("Enable periodic notifcations?"),
                message: Text(ntfModeDescription(mode)),
                primaryButton: .default(Text("Enable")) {
                    notificationMode = mode
                    m.notificationMode = mode
                },
                secondaryButton: .cancel() {
                    notificationMode = m.notificationMode
                }
            )
        case .instant:
            return Alert(
                title: Text("Enable instant notifications?"),
                message: Text(ntfModeDescription(mode)),
                primaryButton: .default(Text("Enable")) {
                    notificationMode = mode
                    m.notificationMode = mode
                },
                secondaryButton: .cancel() {
                    notificationMode = m.notificationMode
                }
            )
        }
    }
}

func ntfModeDescription(_ mode: NotificationMode) -> LocalizedStringKey {
    switch mode {
    case .off: return "**Maximum privacy**: push notifications are off.\nNo meta-data is shared with SimpleX Chat notification server."
    case .periodic: return "**High privacy**: new messages are checked every 20 minutes.\nYour device token is shared with SimpleX Chat notification server, but it cannot see how many connections you have or how many messages you receive."
    case .instant: return "**Medium privacy** (recommended): notifications are sent instantly.\nYour device token and notifications are sent to SimpleX Chat notification server, but it cannot access the message content, size or who is it from."
    }
}

struct SelectionListView<Item: SelectableItem>: View {
    var list: [Item]
    @Binding var selection: Item?
    var onSelection: ((Item) -> Void)?
    @State private var tapped: Item? = nil

    var body: some View {
        ForEach(list) { item in
            HStack {
                Text(item.label)
                Spacer()
                if selection == item {
                    Image(systemName: "checkmark")
                        .resizable().scaledToFit().frame(width: 16)
                        .foregroundColor(.accentColor)
                }
            }
            .contentShape(Rectangle())
            .listRowBackground(Color(uiColor: tapped == item ? .secondarySystemFill : .systemBackground))
            .onTapGesture {
                if let f = onSelection {
                    f(item)
                } else {
                    selection = item
                }
            }
            ._onButtonGesture { down in
                if down {
                    tapped = item
                } else {
                    tapped = nil
                }
            } perform: {}
        }
        .environment(\.editMode, .constant(.active))
    }
}

struct NotificationsView_Previews: PreviewProvider {
    static var previews: some View {
        NotificationsView()
    }
}
