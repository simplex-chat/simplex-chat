class Sem {
  private readonly promises: ((x: unknown) => void)[] = []

  constructor(private permits: number) {}

  signal(): void {
    this.permits += 1
    if (this.promises.length > 0) (this.promises.pop() as () => void)()
  }

  async wait(): Promise<void> {
    if (this.permits === 0 || this.promises.length > 0) {
      await new Promise((r) => this.promises.unshift(r))
    }
    this.permits -= 1
  }
}

export type NextIter<T> = {value: T | Promise<T>; done?: false} | {value?: undefined; done: true}

const queueClosed = Symbol()

type QueueItem<T> = T | typeof queueClosed

export class ABQueueError extends Error {}

export class ABQueue<T> {
  private readonly queue: QueueItem<T>[] = []
  private readonly enq: Sem
  private readonly deq: Sem
  private enqClosed = false
  private deqClosed = false

  constructor(readonly maxSize: number) {
    this.enq = new Sem(0)
    this.deq = new Sem(maxSize)
  }

  [Symbol.asyncIterator](): ABQueue<T> {
    return this
  }

  enqueue(x: T): Promise<void> {
    return this._enqueue(x)
  }

  private async _enqueue(x: QueueItem<T>): Promise<void> {
    if (this.enqClosed) throw new ABQueueError("enqueue: queue closed")
    await this.deq.wait()
    this.queue.push(x)
    this.enq.signal()
  }

  async dequeue(): Promise<T> {
    if (this.deqClosed) throw new ABQueueError("dequeue: queue closed")
    this.deq.signal()
    await this.enq.wait()
    const x = this.queue.shift() as QueueItem<T>
    if (x === queueClosed) {
      this.deqClosed = true
      throw new ABQueueError("dequeue: queue closed")
    }
    return x
  }

  async close(): Promise<void> {
    await this._enqueue(queueClosed)
    this.enqClosed = true
  }

  async next(): Promise<NextIter<T>> {
    if (this.deqClosed) return {done: true}
    try {
      return {value: await this.dequeue()}
    } catch (e) {
      if (e instanceof ABQueueError) return {done: true}
      throw e
    }
  }
}
