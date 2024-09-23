//
//  SwiftUISheet.swift
//  SimpleX (iOS)
//
//  Created by user on 23/09/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SwiftUISheet<SheetContent: View>: ViewModifier {
    let height: Double
    @Binding var isPresented: Bool
    @ViewBuilder let sheetContent: () -> SheetContent
    @Environment(\.colorScheme) private var colorScheme: ColorScheme

    // Represents offset relative to the height of the sheet
    // - 0: Collapsed
    // - 1: Fully expanded
    @State private var relativeOffset: Double = 0
    private let radius: Double = 16

    func body(content: Content) -> some View {
        let safeAreaInset: Double =
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
            let window = windowScene.windows.first {
             window.safeAreaInsets.bottom
         } else { 0 }
        let sheetHeight = height + safeAreaInset
        ZStack {
            content
            Color.black.opacity(0.3 * relativeOffset)
                .ignoresSafeArea()
                .onTapGesture { isPresented = false }
            sheetContent()
                .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
                .background(elevatedSystemGroupedBackground(colorScheme))
                .clipShape(ClipShape(bottomSafeAreaInset: safeAreaInset))
                .frame(height: height)
                .gesture(
                    DragGesture(minimumDistance: 8)
                        .onChanged {
                            relativeOffset = min(max(relativeOffset - $0.translation.height / sheetHeight, 0), 1)
                        }
                        .onEnded { _ in
                            let ip = relativeOffset > 0.6
                            if ip == isPresented {
                                animate()
                            } else {
                                isPresented = ip
                            }
                        }
                )
                .frame(maxHeight: .infinity, alignment: .bottom)
                .offset(y: sheetHeight - sheetHeight * relativeOffset)
        }
        .onChange(of: isPresented) { _ in animate() }
    }

    private var bottomSafeAreaInset: Double {
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
           let window = windowScene.windows.first {
            window.safeAreaInsets.bottom
        } else { 0 }
    }

    private func animate() {
        let newOffset: Double = isPresented ? 1 : 0
        let distance: Double = abs(newOffset - relativeOffset)
        if distance != 0 {
            withAnimation(
                .easeOut(duration: max(0.1, distance * 0.3))
            ) { relativeOffset = isPresented ? 1 : 0 }
        }
    }

    struct ClipShape: Shape {
        let bottomSafeAreaInset: Double

        func path(in rect: CGRect) -> Path {
            Path(
                UIBezierPath(
                    roundedRect: CGRect(
                        origin: .zero,
                        size: CGSize(width: rect.width, height: rect.height + bottomSafeAreaInset)
                    ),
                    byRoundingCorners: [.topLeft, .topRight],
                    cornerRadii: CGSize(width: 10, height: 10)
                ).cgPath
            )
        }
    }
}

func elevatedSystemGroupedBackground(_ colorScheme: ColorScheme) -> Color {
    switch colorScheme {
    case .dark: Color(0xFF1C1C1E)
    default:    Color(0xFFF2F2F7)
    }
}

func elevatedSecondarySystemGroupedBackground(_ colorScheme: ColorScheme) -> Color {
    switch colorScheme {
    case .dark: Color(0xFF2C2C2E)
    default:    Color(0xFFFFFFFF)
    }
}

/// # Extracting Sheet Colors Programatically
///
/// System colors are returned dynamically, depending on the context:
///
///     struct ColorResolverView: View {
///         @Environment(\.self) var environment
///         let colors: [Color]
///
///         var body: some View {
///             HStack {
///                 column.environment(\.colorScheme, .dark)
///                 column.environment(\.colorScheme, .light)
///             }
///         }
///
///         var column: some View {
///             VStack {
///                 ForEach(colors, id: \.self) {
///                     Text("\($0.resolve(in: environment))")
///                         .frame(maxWidth: .infinity, maxHeight: .infinity)
///                         .background($0)
///                 }
///             }
///           }
///     }
///
/// Place `ColorResolverView` inside a sheet to acquire elevated color versions:
///
///     struct ContentView: View {
///         var body: some View {
///             EmptyView()
///             .sheet(isPresented: .constant(true)) {
///                 ColorResolverView(
///                     colors: [
///                         Color(.systemGroupedBackground),
///                         Color(.secondarySystemGroupedBackground)
///                     ]
///                 )
///             }
///         }
///     }


