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
        case missingAttachment
        case noChatWasSelected
        case loadFileRepresentation(Error)
        case encryptFileFailure
        case sendMessageFailure

        public var errorDescription: String? {
            switch self {
            case .missingAttachment: 
                "Missing Attachment"
            case .noChatWasSelected: 
                "No chat was selected"
            case .loadFileRepresentation(let error): 
                "File could not be loaded: \(error.localizedDescription)"
            case .encryptFileFailure: 
                "File encryption has failed"
            case .sendMessageFailure: 
                "Message has failed to be sent"
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
