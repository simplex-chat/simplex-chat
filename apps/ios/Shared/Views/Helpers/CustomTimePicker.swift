//
//  CustomTimePicker.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.05.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

public enum SelectableTimeInterval {
    case seconds
    case minutes
    case hours
    case days
    case weeks

    public var toSeconds: Int {
        switch self {
        case .seconds: return 1
        case .minutes: return 60
        case .hours: return 3600
        case .days: return 86400
        case .weeks: return 7 * 86400
        }
    }

    public var selectableValues: [Int] {
        switch self {
        case .seconds: return Array(1...59)
        case .minutes: return Array(1...59)
        case .hours: return Array(1...23)
        case .days: return Array(1...6)
        case .weeks: return Array(1...4)
        }
    }

    public var text: String {
        switch self {
        case .seconds: return NSLocalizedString("seconds", comment: "selectable time interval")
        case .minutes: return NSLocalizedString("minutes", comment: "selectable time interval")
        case .hours: return NSLocalizedString("hours", comment: "selectable time interval")
        case .days: return NSLocalizedString("days", comment: "selectable time interval")
        case .weeks: return NSLocalizedString("weeks", comment: "selectable time interval")
        }
    }

    public static func toSelectedTimeInterval(seconds: Int) -> (SelectableTimeInterval, Int) {
        let tryIntervals = [
            SelectableTimeInterval.weeks,
            SelectableTimeInterval.days,
            SelectableTimeInterval.hours,
            SelectableTimeInterval.minutes
        ]
        var selectedInterval: (SelectableTimeInterval, Int)? = nil
        for interval in tryIntervals {
            let (v, r) = divMod(seconds, by: interval.toSeconds)
            if r == 0 {
                selectedInterval = (interval, v)
                break
            }
        }
        return selectedInterval ?? (SelectableTimeInterval.seconds, seconds)
    }
}

struct CustomTimePicker: View {
    @Binding var selection: Int?
    @State private var allowedTimeIntervals = [
        SelectableTimeInterval.weeks,
        SelectableTimeInterval.days,
        SelectableTimeInterval.hours,
        SelectableTimeInterval.minutes,
        SelectableTimeInterval.seconds
    ]
    @State private var selectedTimeInterval: SelectableTimeInterval = .seconds
    @State private var selectedValue: Int = 1

    var body: some View {
        HStack(spacing: 0) {
            Group {
                Picker("Interval", selection: $selectedTimeInterval) {
                    ForEach(allowedTimeIntervals, id: \.self) { interval in
                        Text(interval.text)
                    }
                }
                Picker("Value", selection: $selectedValue) {
                    let stiValues = selectedTimeInterval.selectableValues
                    let values = stiValues + (stiValues.contains(selectedValue) ? [] : [selectedValue])
                    ForEach(values, id: \.self) { value in
                        Text("\(value)")
                    }
                }
            }
            .pickerStyle(.wheel)
            .frame(minWidth: 0)
            .compositingGroup()
            .clipped()
        }
        .padding()
        .onAppear {
            if let selection = selection,
               selection > 0 {
                (selectedTimeInterval, selectedValue) = SelectableTimeInterval.toSelectedTimeInterval(seconds: selection)
            }
        }
        .onChange(of: selectedTimeInterval) { interval in
            if let maxValue = interval.selectableValues.last,
               selectedValue > maxValue {
                selectedValue = maxValue
            } else {
                selection = interval.toSeconds * selectedValue
            }
        }
        .onChange(of: selectedValue) { value in
            selection = selectedTimeInterval.toSeconds * value
        }
    }
}

struct CustomTimePicker_Previews: PreviewProvider {
    static var previews: some View {
        CustomTimePicker(
            selection: Binding.constant(30)
        )
    }
}
