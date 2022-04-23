//
//  PasteToConnectView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 22/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PasteToConnectView: View {
    var connectViaLink: (String) -> Void
    @State private var teHeight: CGFloat = 42
    @State private var connectionLink: String = ""
    @Namespace var namespace
    var maxHeight: CGFloat = 360
    var minHeight: CGFloat = 37

    var body: some View {
        ZStack {
            HStack(alignment: .bottom) {
                ZStack(alignment: .leading) {
                    Text(connectionLink)
                        .font(.body)
                        .foregroundColor(.clear)
                        .padding(.horizontal, 10)
                        .padding(.vertical, 8)
                        .matchedGeometryEffect(id: "te", in: namespace)
                        .background(GeometryReader(content: updateHeight))
                    TextEditor(text: $connectionLink)
                        .onSubmit(submit)
                        .font(.body)
                        .textInputAutocapitalization(.never)
                        .padding(.horizontal, 5)
                        .allowsTightening(false)
                        .frame(height: teHeight)
                }
                Button(action: submit) {
                    Image(systemName: (connectionLink != "") ? "checkmark.circle.fill" : "arrow.right.doc.on.clipboard")
                        .resizable()
                        .foregroundColor(.accentColor)
                }
                .frame(width: 29, height: 29)
                .padding([.bottom, .trailing], 4)
            }

            RoundedRectangle(cornerSize: CGSize(width: 20, height: 20))
                .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                .frame(height: teHeight)
        }
        .padding(.vertical, 8)
    }
    private func submit() {
        if (connectionLink != "") {
            connectViaLink(connectionLink)
            connectionLink = ""
        } else {
            connectionLink = UIPasteboard.general.string ?? ""
        }
    }
    func updateHeight(_ g: GeometryProxy) -> Color {
        DispatchQueue.main.async {
            teHeight = min(max(g.frame(in: .local).size.height, minHeight), maxHeight)
        }
        return Color.clear
    }
}

struct PasteToConnectView_Previews: PreviewProvider {
    static var previews: some View {
        return PasteToConnectView(connectViaLink: { print($0) })
    }
}
