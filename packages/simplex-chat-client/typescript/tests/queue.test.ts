import * as assert from "assert"
import {ABQueue, ABQueueError} from "../src/queue"

describe("ABQueue", () => {
  test("async queue API", async () => {
    const arr = makeArr(100)
    await testQueue(10)
    await testQueue(1)
    await testQueue(0)

    async function testQueue(maxSize: number): Promise<void> {
      const q = new ABQueue<number>(maxSize)
      const res = await Promise.all([enqueueArr(q, arr), dequeueArr(q)])
      assert.deepStrictEqual(arr, res[1])
    }
  })

  test("async queue iterator API", async () => {
    const arr = makeArr(100)
    await testQueueIterator(10)
    await testQueueIterator(1)
    await testQueueIterator(0)

    async function testQueueIterator(maxSize: number): Promise<void> {
      const q = new ABQueue<number>(maxSize)
      const res = await Promise.all([enqueueArr(q, arr), iterQueue(q)])
      assert.deepStrictEqual(arr, res[1])
    }
  })

  test("enqueue / dequeue with closed queue throws exception", async () => {
    const q = new ABQueue<number>(10)
    await q.enqueue(1)
    await q.enqueue(2)
    await q.close()
    await assert.rejects(q.enqueue(3))
    assert.strictEqual(await q.dequeue(), 1)
    assert.strictEqual(await q.dequeue(), 2)
    await assert.rejects(q.dequeue())
    await assert.rejects(q.enqueue(3))
    await assert.rejects(q.dequeue())
    assert.deepStrictEqual(await q.next(), {done: true})
  })
})

function makeArr(size: number): number[] {
  return Array(size)
    .fill(0)
    .map((_, i) => i)
}

async function enqueueArr(q: ABQueue<number>, xs: number[]): Promise<void> {
  for (const x of xs) {
    await q.enqueue(x)
  }
  await q.close()
}

async function dequeueArr(q: ABQueue<number>): Promise<number[]> {
  const xs: number[] = []
  // eslint-disable-next-line no-constant-condition
  while (true) {
    try {
      xs.push(await q.dequeue())
    } catch (e) {
      assert.ok(e instanceof ABQueueError)
      break
    }
  }
  return xs
}

async function iterQueue(q: ABQueue<number>): Promise<number[]> {
  const xs: number[] = []
  for await (const x of q) xs.push(await x)
  return xs
}
