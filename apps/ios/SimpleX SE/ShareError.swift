//
//  ShareErr.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 11/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

enum ShareError: LocalizedError {
    case sendMessage(SendMessage)
    case fetchChats(FetchChats)

    var errorDescription: String? {
        switch self {
        case let .sendMessage(error): error.errorDescription
        case let .fetchChats(error): error.errorDescription
        }
    }

    enum SendMessage: LocalizedError {
        case noChatWasSelected
        case missingAttachment
        case unsupportedFormat
        case imageDecoding
        case encryptFile
        case previewGeneration
        case sendMessage
        case loadFileRepresentation(Error)

        public var errorDescription: String? {
            switch self {
            case .noChatWasSelected:
                "No chat was selected"
            case .missingAttachment:
                "No file has been selected"
            case .unsupportedFormat:
                "This file format is not supported"
            case .imageDecoding:
                "Unable to decode image"
            case .encryptFile:
                "File encryption has failed"
            case .previewGeneration:
                "Thumbnail generation has failed"
            case .sendMessage:
                "Message has failed to be sent"
            case .loadFileRepresentation(let error):
                "File could not be loaded: \(error.localizedDescription)"
            }
        }
    }

    enum FetchChats: LocalizedError {
        case unexpectedMigrationResult(DBMigrationResult)
        case noActiveUser
        case networkConfigurationFailure
        case unableToSetupFilePaths
        case unableToStartChat
        case unableToFetchChats(User)

        public var errorDescription: String? {
            switch self {
            case let .unexpectedMigrationResult(result):
                "Encountered Database Issue: \(result)"
            case .noActiveUser:
                "User has not been found"
            case .networkConfigurationFailure:
                "Encountered network configuration issue"
            case .unableToSetupFilePaths:
                "Encountered issue setting up file paths"
            case .unableToStartChat:
                "Unable to start the chat"
            case let .unableToFetchChats(user):
                "Failed to fetch chats for \(user.displayName)"
            }
        }
    }
}
