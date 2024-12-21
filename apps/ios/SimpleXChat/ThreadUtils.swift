//
//  ThreadExecutor.swift
//  SimpleX
//
//  Created by acevif on 2024/09/12.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation

class ThreadExecutor<ResultType> {
    private var stackSize: Int
    private var qos: QualityOfService

    /// Initialize by specifying the stack size
    init(stackSize: Int, qos: QualityOfService) {
        self.stackSize = stackSize
        self.qos = qos
    }

    /// Execute a closure synchronously on a separate thread and return the result
    func executeSync(_ task: @escaping () throws -> ResultType) throws -> ResultType {
        // Initialize the semaphore (initially locked)
        let semaphore = DispatchSemaphore(value: 0)
        var result: Result<ResultType, Error>?

        // Initialize the thread
        let thread = Thread {
            do {
                let taskResult = try task()
                result = .success(taskResult)
            } catch {
                result = .failure(error)
            }
            // Release the semaphore when the task is completed
            semaphore.signal()
        }

        thread.stackSize = stackSize
        thread.qualityOfService = qos

        thread.start()

        // Wait until the thread completes
        semaphore.wait()

        // Return the result
        switch result! {
        case .success(let result):
            return result
        case .failure(let error):
            throw error
        }
    }
}
