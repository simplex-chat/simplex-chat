//
//  ConcurrentQueue.swift
//  SimpleX NSE
//
//  Created by Evgeny on 08/12/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import Foundation

struct DequeueElement<T> {
    var elementId: UUID?
    var task: Task<T?, Never>
}

class ConcurrentQueue<T> {
    private var queue: [T] = []
    private var queueLock = DispatchQueue(label: "chat.simplex.app.SimpleX-NSE.concurrent-queue.lock.\(UUID())")
    private var continuations = [(elementId: UUID, continuation: CheckedContinuation<T?, Never>)]()

    func enqueue(_ el: T) {
        resumeContinuation(el) { self.queue.append(el) }
    }

    func frontEnqueue(_ el: T) {
        resumeContinuation(el) { self.queue.insert(el, at: 0) }
    }

    private func resumeContinuation(_ el: T, add: @escaping () -> Void) {
        queueLock.sync {
            if let (_, cont) = continuations.first {
                continuations.remove(at: 0)
                cont.resume(returning: el)
            } else {
                add()
            }
        }
    }

    func dequeue() -> DequeueElement<T> {
        queueLock.sync {
            if queue.isEmpty {
                let elementId = UUID()
                let task = Task {
                    await withCheckedContinuation { cont in
                        continuations.append((elementId, cont))
                    }
                }
                return DequeueElement(elementId: elementId, task: task)
            } else {
                return DequeueElement(task: Task { queue.remove(at: 0) })
            }
        }
    }

    func cancelDequeue(_ elementId: UUID) {
        let match: (((elementId: UUID, CheckedContinuation<T?, Never>)) -> Bool) = { $0.elementId == elementId }
        queueLock.sync {
            let cancelled = continuations.filter(match)
            continuations.removeAll(where: match)
            cancelled.forEach { $0.continuation.resume(returning: nil) }
        }
    }
}
