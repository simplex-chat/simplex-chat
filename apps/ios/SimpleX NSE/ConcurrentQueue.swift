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
                let el = queue.remove(at: 0)
                return DequeueElement(task: Task { el })
            }
        }
    }

    func cancelDequeue(_ elementId: UUID) {
        queueLock.sync {
            let cancelled = continuations.filter { $0.elementId == elementId }
            continuations.removeAll { $0.elementId == elementId }
            cancelled.forEach { $0.continuation.resume(returning: nil) }
        }
    }
}
