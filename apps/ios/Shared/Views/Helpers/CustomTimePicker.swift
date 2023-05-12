//
//  CustomTimePicker.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.05.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CustomTimePicker: View {
    @Binding var selection: Int?
    @State var timeUnitsLimits = TimeUnitLimits.defaultUnitsLimits
    @State private var selectedUnit: TimeUnit = .second
    @State private var selectedDuration: Int = 1

    enum TimeUnit {
        case second
        case minute
        case hour
        case day
        case week
        case month

        public var toSeconds: Int {
            switch self {
            case .second: return 1
            case .minute: return 60
            case .hour: return 3600
            case .day: return 86400
            case .week: return 7 * 86400
            case .month: return 30 * 86400
            }
        }

        public var text: String {
            switch self {
            case .second: return NSLocalizedString("seconds", comment: "time unit")
            case .minute: return NSLocalizedString("minutes", comment: "time unit")
            case .hour: return NSLocalizedString("hours", comment: "time unit")
            case .day: return NSLocalizedString("days", comment: "time unit")
            case .week: return NSLocalizedString("weeks", comment: "time unit")
            case .month: return NSLocalizedString("months", comment: "time unit")
            }
        }

        public var defaultLimits: TimeUnitLimits {
            switch self {
            case .second: return TimeUnitLimits.init(timeUnit: .second, maxValue: 120)
            case .minute: return TimeUnitLimits.init(timeUnit: .minute, maxValue: 120)
            case .hour: return TimeUnitLimits.init(timeUnit: .hour, maxValue: 72)
            case .day: return TimeUnitLimits.init(timeUnit: .day, maxValue: 30)
            case .week: return TimeUnitLimits.init(timeUnit: .week, maxValue: 14)
            case .month: return TimeUnitLimits.init(timeUnit: .month, maxValue: 12)
            }
        }

        public static func toSelectedTimeUnit(seconds: Int) -> (TimeUnit, Int) {
            let tryIntervals = [
                TimeUnit.month,
                TimeUnit.week,
                TimeUnit.day,
                TimeUnit.hour,
                TimeUnit.minute
            ]
            var selectedInterval: (TimeUnit, Int)? = nil
            for interval in tryIntervals {
                let (v, r) = divMod(seconds, by: interval.toSeconds)
                if r == 0 {
                    selectedInterval = (interval, v)
                    break
                }
            }
            return selectedInterval ?? (TimeUnit.second, seconds)
        }
    }

    struct TimeUnitLimits {
        var timeUnit: TimeUnit
        var minValue: Int = 1
        var maxValue: Int

        public static var defaultUnitsLimits: [TimeUnitLimits] {[
            TimeUnit.second.defaultLimits,
            TimeUnit.minute.defaultLimits,
            TimeUnit.hour.defaultLimits,
            TimeUnit.day.defaultLimits,
            TimeUnit.week.defaultLimits,
            TimeUnit.month.defaultLimits,
        ]}
    }

    var body: some View {
        HStack(spacing: 0) {
            Group {
                Picker("Unit", selection: $selectedUnit) {
                    ForEach(timeUnitsLimits.map { $0.timeUnit }, id: \.self) { timeUnit in
                        Text(timeUnit.text)
                    }
                }
                Picker("Duration", selection: $selectedDuration) {
                    let selectedUnitLimits = timeUnitsLimits.first(where: { $0.timeUnit == selectedUnit }) ?? selectedUnit.defaultLimits
                    let selectedUnitValues = Array(selectedUnitLimits.minValue...selectedUnitLimits.maxValue)
                    let values = selectedUnitValues + (selectedUnitValues.contains(selectedDuration) ? [] : [selectedDuration])
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
        .onAppear {
            if let selection = selection,
               selection > 0 {
                (selectedUnit, selectedDuration) = TimeUnit.toSelectedTimeUnit(seconds: selection)
            }
        }
        .onChange(of: selectedUnit) { unit in
            if let maxValue = timeUnitsLimits.first(where: { $0.timeUnit == unit })?.maxValue,
               selectedDuration > maxValue {
                selectedDuration = maxValue
            } else {
                selection = unit.toSeconds * selectedDuration
            }
        }
        .onChange(of: selectedDuration) { duration in
            selection = selectedUnit.toSeconds * duration
        }
    }
}

extension UIPickerView {
    open override var intrinsicContentSize: CGSize {
        return CGSize(width: UIView.noIntrinsicMetric, height: super.intrinsicContentSize.height)
    }
}

struct CustomTimePickerView: View {
    @Environment(\.dismiss) var dismiss
    var confirmButtonText: LocalizedStringKey
    var confirmButtonAction: () -> Void
    var description: LocalizedStringKey? = nil
    @Binding var selection: Int?
    @State var timeUnitsLimits = CustomTimePicker.TimeUnitLimits.defaultUnitsLimits

    var body: some View {
        NavigationView {
            customTimePickerView()
                .toolbar {
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Button {
                            confirmButtonAction()
                            dismiss()
                        } label: {
                            Text(confirmButtonText)
                                .fontWeight(.medium)
                        }
                    }
                    ToolbarItem(placement: .navigationBarLeading) {
                        Button("Cancel") {
                            dismiss()
                        }
                    }
                }
        }
    }

    private func customTimePickerView() -> some View {
        VStack(alignment: .leading) {
            List {
                Group {
                    if let description = description {
                        Text(description)
                            .listRowBackground(Color.clear)
                    }
                    Section {
                        CustomTimePicker(selection: $selection)
                    }
                }
                .listRowInsets(.init(top: 0, leading: 16, bottom: 0, trailing: 16))
            }
        }
    }
}

struct CustomTimePicker_Previews: PreviewProvider {
    static var previews: some View {
        CustomTimePicker(
            selection: Binding.constant(300)
        )
    }
}
