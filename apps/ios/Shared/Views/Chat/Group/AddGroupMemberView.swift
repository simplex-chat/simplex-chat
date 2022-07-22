//
//  AddGroupMemberView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 22.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct AddGroupMemberView: View {
    var body: some View {
        List {
            Text("Alice")
            Text("Bob")
            Text("Catherine")
        }
    }
}

struct AddGroupMemberView_Previews: PreviewProvider {
    static var previews: some View {
        AddGroupMemberView()
    }
}
