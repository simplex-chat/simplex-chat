//
//  LinkListRowView.swift
//  SimpleX
//
//  Created by Suren Poghosyan on 20.01.26.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import LinkPresentation

struct LinkListRowView: View {
 
    let chatItem: ChatItem
    
    @EnvironmentObject var theme: AppTheme
    
    @State private var linkThumbnail: UIImage? = nil
    
    var body: some View {
        HStack(spacing: 12) {
            if let image = linkThumbnail {
                Image(uiImage: image)
                    .resizable()
                    .aspectRatio(contentMode: .fill)
                    .clipShape(Circle())
                    .frame(width: 40, height: 40)
            } else {
                ZStack {
                    Circle()
                        .fill(Color(.systemGray5))
                        .frame(width: 40, height: 40)
                    
                    Image(systemName: "link")
                        .font(.system(size: 20))
                        .foregroundColor(theme.colors.primary)
                }
            }
            
            VStack(alignment: .leading, spacing: 4) {
                Text(linkText)
                    .font(.body)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(2)
                
                Text(formatTimestampChatInfo(chatItem.meta.itemTs))
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            
            Spacer()
            
            Image(systemName: "chevron.right")
                .font(.system(size: 14))
                .foregroundColor(theme.colors.secondary)
        }
        .padding(.vertical, 8)
        .onAppear {
            loadThumbnail()
        }
    }
    
    private var linkText: String {
        chatItem.content.text
    }
    
    private var linkURL: URL? {
        URL(string: linkText.trimmingCharacters(in: .whitespacesAndNewlines))
    }
    
    private func loadThumbnail() {
        guard linkURL != nil, linkThumbnail == nil else { return }
        
        let provider = LPMetadataProvider()
        provider.startFetchingMetadata(for: linkURL!) { metadata, error in
            guard error == nil, let metadata = metadata else { return }
            if let imageProvider = metadata.imageProvider {
                imageProvider.loadObject(ofClass: UIImage.self) { image, error in
                    if let uiImage = image as? UIImage {
                        DispatchQueue.main.async {
                            self.linkThumbnail = uiImage
                        }
                    }
                }
            }
        }
    }
}
