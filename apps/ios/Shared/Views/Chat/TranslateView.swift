//
//  TranslateView.swift
//  SimpleX
//
//  Created by user on 19/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import Translation

@available(iOS 18.0, *)
struct TranslateView: View {
    let source: String
    @State private var supprtedLanguages = [Locale.Language]()
    @State private var target: String?
    @State private var configuration = TranslationSession.Configuration()

    var body: some View {
        NavigationStack {
            List {
                Section {
                    languagePicker("From", selection: $configuration.source)
                    Text(source).foregroundStyle(.secondary).lineLimit(1)
                }
                Section {
                    languagePicker("To ", selection: $configuration.target)
                    if let target {
                        Text(target)
                    } else {
                        ProgressView()
                    }
                } footer: {
                    Text("\(Image(systemName: "info.circle")) Uses on device translation")
                }
            }
            .navigationTitle("Translate")
        }
        .task { supprtedLanguages = await LanguageAvailability().supportedLanguages }
        .translationTask(configuration) { session in
            do {
                target = nil
                let response = try await session.translate(source)
                await MainActor.run {
                    configuration.source = response.sourceLanguage
                    configuration.target = response.targetLanguage
                    target = response.targetText
                }
            } catch {
                print(error)
            }
        }
    }

    @ViewBuilder
    private func languagePicker(_ label: String, selection: Binding<Locale.Language?>) -> some View {
        Picker(label, selection: selection) {
            Text("Detect language").tag(Optional<Locale.Language>.none)
            ForEach(supprtedLanguages, id: \.self) { language in
                Text(title(for: language)).tag(language)
            }
        }
    }

    private func title(for language: Locale.Language) -> String {
        if let code = language.languageCode,
           let string = Locale.current.localizedString(forLanguageCode: code.identifier) {
            string
        } else {
            language.minimalIdentifier
        }
    }
}
