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
    @State private var selectedUnit: CustomTimeUnit = .second
    @State private var selectedDuration: Int = 1

    struct TimeUnitLimits {
        var timeUnit: CustomTimeUnit
        var minValue: Int = 1
        var maxValue: Int

        public static func defaultUnitLimits(_ unit: CustomTimeUnit) -> TimeUnitLimits {
            switch unit {
            case .second: return TimeUnitLimits.init(timeUnit: .second, maxValue: 120)
            case .minute: return TimeUnitLimits.init(timeUnit: .minute, maxValue: 120)
            case .hour: return TimeUnitLimits.init(timeUnit: .hour, maxValue: 72)
            case .day: return TimeUnitLimits.init(timeUnit: .day, maxValue: 60)
            case .week: return TimeUnitLimits.init(timeUnit: .week, maxValue: 52)
            case .month: return TimeUnitLimits.init(timeUnit: .month, maxValue: 12)
            }
        }

        public static var defaultUnitsLimits: [TimeUnitLimits] {[
            defaultUnitLimits(.second),
            defaultUnitLimits(.minute),
            defaultUnitLimits(.hour),
            defaultUnitLimits(.day),
            defaultUnitLimits(.week),
            defaultUnitLimits(.month),
        ]}
    }

    var body: some View {
        HStack(spacing: 0) {
            Group {
                Picker("Duration", selection: $selectedDuration) {
                    let selectedUnitLimits = timeUnitsLimits.first(where: { $0.timeUnit == selectedUnit }) ?? TimeUnitLimits.defaultUnitLimits(selectedUnit)
                    let selectedUnitValues = Array(selectedUnitLimits.minValue...selectedUnitLimits.maxValue)
                    let values = selectedUnitValues + (selectedUnitValues.contains(selectedDuration) ? [] : [selectedDuration])
                    ForEach(values, id: \.self) { value in
                        Text("\(value)")
                    }
                }
                Picker("Unit", selection: $selectedUnit) {
                    ForEach(timeUnitsLimits.map { $0.timeUnit }, id: \.self) { timeUnit in
                        Text(timeUnit.text)
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
                (selectedUnit, selectedDuration) = CustomTimeUnit.toTimeUnit(seconds: selection)
            } else {
                selection = selectedUnit.toSeconds * selectedDuration
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
    @Binding var selection: Int?
    var confirmButtonText: LocalizedStringKey
    var confirmButtonAction: () -> Void
    var description: LocalizedStringKey? = nil
    var timeUnitsLimits = CustomTimePicker.TimeUnitLimits.defaultUnitsLimits

    var body: some View {
        NavigationView {
            customTimePickerView()
                .toolbar {
                    ToolbarItem(placement: .navigationBarLeading) {
                        Button("Cancel") {
                            dismiss()
                        }
                    }
                    ToolbarItem(placement: .navigationBarTrailing) {
                        Button {
                            confirmButtonAction()
                            dismiss()
                        } label: {
                            Text(confirmButtonText)
                                .fontWeight(.medium)
                        }
                        .disabled(selection == nil)
                    }
                }
        }
    }

    private func customTimePickerView() -> some View {
        VStack(alignment: .leading) {
            List {
                Group {
                    Section(description ?? "") {
                        CustomTimePicker(selection: $selection)
                    }
                }
                .listRowInsets(.init(top: 0, leading: 16, bottom: 0, trailing: 16))
            }
            .listStyle(.insetGrouped)
        }
    }
}

struct DropdownCustomTimePicker: View {
    @Binding var selection: Int?
    var label: LocalizedStringKey
    var dropdownValues: [Int?]
    var customPickerConfirmButtonText: LocalizedStringKey
    var customPickerDescription: LocalizedStringKey? = nil
    var customPickerTimeUnitsLimits = CustomTimePicker.TimeUnitLimits.defaultUnitsLimits
    @State private var dropdownSelection: DropdownSelection = .dropdownValue(value: nil)
    @State private var showCustomTimePicker = false
    @State private var selectedCustomTime: Int? = nil
    @State private var justOpened = true

    enum DropdownSelection: Hashable {
        case dropdownValue(value: Int?)
        case custom
    }

    var body: some View {
        Picker(label, selection: $dropdownSelection) {
            let values: [DropdownSelection] =
            dropdownValues.map { .dropdownValue(value: $0) }
            + (dropdownValues.contains(selection) ? [] : [.dropdownValue(value: selection)])
            + [.custom]
            ForEach(values, id: \.self) { v in
                switch v {
                case let .dropdownValue(value): Text(timeText(value))
                case .custom: Text(NSLocalizedString("custom", comment: "dropdown time picker choice"))
                }
            }
        }
        .onAppear {
            if #unavailable(iOS 16) {
                // this condition prevents re-setting picker
                if !justOpened { return }
            }
            dropdownSelection = .dropdownValue(value: selection)
            justOpened = false
        }
        .onChange(of: selection) { v in
            logger.debug("*** .onChange(of: selection)")
            dropdownSelection = .dropdownValue(value: v)
        }
        .onChange(of: dropdownSelection) { v in
            logger.debug("*** .onChange(of: dropdownSelection)")
            switch v {
            case let .dropdownValue(value): selection = value
            case .custom: showCustomTimePicker = true
            }
        }
        .sheet(
            isPresented: $showCustomTimePicker,
            onDismiss: {
                dropdownSelection = .dropdownValue(value: selection)
                selectedCustomTime = nil
            }
        ) {
            if #available(iOS 16.0, *) {
                customTimePicker()
                    .presentationDetents([.medium])
            } else {
                customTimePicker()
            }
        }
    }

    private func customTimePicker() -> some View {
        CustomTimePickerView(
            selection: $selectedCustomTime,
            confirmButtonText: customPickerConfirmButtonText,
            confirmButtonAction: {
                if let time = selectedCustomTime {
                    selection = time
                }
            },
            description: customPickerDescription,
            timeUnitsLimits: customPickerTimeUnitsLimits
        )
        .onAppear {
            selectedCustomTime = selection
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
