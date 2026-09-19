import {execFile, spawnSync} from "child_process";
import * as fs from "fs";
import * as path from "path";
import {core} from "../src/index";
import * as libs from "../src/libs";

describe("Core tests", () => {
  beforeAll(() => core.loadLibrary("sqlite"));
  const tmpDir = "./tests/tmp";
  const dbPath = path.join(tmpDir, "simplex_v1");

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}));
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}));

  async function stopAndClose(ctrl: bigint): Promise<void> {
    await expect(core.chatSendCmd(ctrl, "/_stop")).resolves.toMatchObject({type: "chatStopped"});
    await core.chatCloseStore(ctrl);
  }

  it("should initialize chat controller", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    expect(typeof ctrl).toBe("bigint");
    await expect(stopAndClose(ctrl)).resolves.toBe(undefined);
    
    await expect(core.chatMigrateInit(dbPath, "wrong_key", core.MigrationConfirmation.YesUp)).rejects.toMatchObject({
      message: "Database or migration error (see dbMigrationError property)",
      dbMigrationError: expect.objectContaining({type: "errorNotADatabase"})
    });
  });

  it("should initialize chat controller with queue size", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp, 65536);
    expect(typeof ctrl).toBe("bigint");
    await expect(stopAndClose(ctrl)).resolves.toBe(undefined);

    await expect(core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp, 0)).rejects.toMatchObject({
      dbMigrationError: {type: "invalidQueueSize"}
    });
    await expect(core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp, 2 ** 31)).rejects.toThrow("Expected 32-bit integer queue size");
    await expect(core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp, 1.5)).rejects.toThrow("Expected 32-bit integer queue size");
  });

  it("should send command and receive event", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);

    await expect(core.chatSendCmd(ctrl, "/v")).resolves.toMatchObject({
      type: "versionInfo"
    });
    await expect(core.chatSendCmd(ctrl, '/debug event {"type": "chatSuspended"}')).resolves.toMatchObject({
      type: "cmdOk"
    });

    const wait = 500_000;
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toMatchObject({
      type: "chatSuspended"
    });    
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toBe(undefined);
    
    await expect(core.chatSendCmd(ctrl, "/unknown")).rejects.toMatchObject({
      message: "Chat command error (see chatError property)",
      chatError: expect.objectContaining({type: "error"})
    });

    await stopAndClose(ctrl);
  });

  it("should write/read encrypted file from/to buffer", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);

    const filePath = path.join(tmpDir, "write_file.txt");
    const buffer = new Uint8Array([0, 1, 2]).buffer;
    const cryptoArgs = await core.chatWriteFile(ctrl, filePath, buffer);
    expect(typeof cryptoArgs.fileKey).toBe("string");
    expect(typeof cryptoArgs.fileNonce).toBe("string");

    const buffer2 = await core.chatReadFile(filePath, cryptoArgs);
    expect(Buffer.from(buffer2).equals(Buffer.from(buffer))).toBe(true);

    await expect(core.chatWriteFile(ctrl, path.join(tmpDir, "unknown", "unknown.txt"), buffer)).rejects.toThrow();
    await expect(core.chatReadFile(path.join(tmpDir, "unknown.txt"), cryptoArgs)).rejects.toThrow();
    
    await stopAndClose(ctrl);
  });

  it("should write the view of a Uint8Array and read an empty file", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);

    const viewPath = path.join(tmpDir, "view.txt");
    const viewArgs = await core.chatWriteFile(ctrl, viewPath, Buffer.from("xxabcxx").subarray(2, 5));
    const view = await core.chatReadFile(viewPath, viewArgs);
    expect(Buffer.isBuffer(view)).toBe(true);
    expect(view.toString()).toBe("abc");

    const emptyPath = path.join(tmpDir, "empty.txt");
    const emptyArgs = await core.chatWriteFile(ctrl, emptyPath, new Uint8Array(0));
    const empty = await core.chatReadFile(emptyPath, emptyArgs);
    expect(Buffer.isBuffer(empty)).toBe(true);
    expect(empty.length).toBe(0);

    await stopAndClose(ctrl);
  });

  it("should encrypt/decrypt file", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);

    const unencryptedPath = path.join(tmpDir, "file_unencrypted.txt");
    fs.writeFileSync(unencryptedPath, "unencrypted\n");
    const encryptedPath = path.join(tmpDir, "file_encrypted.txt");
    const cryptoArgs = await core.chatEncryptFile(ctrl, unencryptedPath, encryptedPath);
    expect(typeof cryptoArgs.fileKey).toBe("string");
    expect(typeof cryptoArgs.fileNonce).toBe("string");

    const decryptedPath: string = path.join(tmpDir, "file_decrypted.txt");
    await expect(core.chatDecryptFile(encryptedPath, cryptoArgs, decryptedPath)).resolves.toBe(undefined);

    expect(fs.readFileSync(decryptedPath, "utf8")).toBe("unencrypted\n");

    await expect(core.chatEncryptFile(ctrl, path.join(tmpDir, "unknown.txt"), encryptedPath)).rejects.toThrow();
    await expect(core.chatDecryptFile(path.join(tmpDir, "unknown.txt"), cryptoArgs, decryptedPath)).rejects.toThrow();
    
    await stopAndClose(ctrl);
  });

  it("should not block the libuv pool while receiving", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    const receives = [1, 2, 3, 4].map(() => core.chatRecvMsgWait(ctrl, 2_000_000));
    const start = Date.now();
    await fs.promises.stat(tmpDir);
    expect(Date.now() - start).toBeLessThan(200);
    await Promise.all(receives);
    await stopAndClose(ctrl);
  }, 10000);

  const itOnLinux = process.platform === "linux" ? it : it.skip;

  // Thread count is read from /proc/self/task, which only exists on Linux.
  itOnLinux("should keep the thread count constant while receiving", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    for (let i = 0; i < 10; i++) await core.chatRecvMsgWait(ctrl, 1);
    const threadCount = () => fs.readdirSync("/proc/self/task").length;
    const warmCount = threadCount();
    const counts = new Set<number>();
    for (let i = 0; i < 200; i++) {
      await core.chatRecvMsgWait(ctrl, 1);
      counts.add(threadCount());
    }
    expect({warmCount, counts: [...counts]}).toEqual({warmCount, counts: [warmCount]});
    await stopAndClose(ctrl);
  }, 30000);

  it("should receive on two controllers concurrently", async () => {
    const ctrlA = await core.chatMigrateInit(path.join(tmpDir, "simplex_a"), "key", core.MigrationConfirmation.YesUp);
    const ctrlB = await core.chatMigrateInit(path.join(tmpDir, "simplex_b"), "key", core.MigrationConfirmation.YesUp);
    const start = Date.now();
    await expect(Promise.all([core.chatRecvMsgWait(ctrlA, 1_000_000), core.chatRecvMsgWait(ctrlB, 1_000_000)]))
      .resolves.toEqual([undefined, undefined]);
    const elapsed = Date.now() - start;
    expect(elapsed).toBeGreaterThanOrEqual(900);
    expect(elapsed).toBeLessThan(1800);
    await stopAndClose(ctrlA);
    await stopAndClose(ctrlB);
  }, 10000);

  it("should receive events of one controller in order", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    for (const action of ["first", "second"]) {
      await expect(core.chatSendCmd(ctrl, `/debug event {"type": "timedAction", "action": "${action}", "durationMilliseconds": 1}`))
        .resolves.toMatchObject({type: "cmdOk"});
    }
    const events = await Promise.all([core.chatRecvMsgWait(ctrl, 500_000), core.chatRecvMsgWait(ctrl, 500_000)]);
    expect(events).toMatchObject([
      {type: "timedAction", action: "first"},
      {type: "timedAction", action: "second"}
    ]);
    await stopAndClose(ctrl);
  }, 10000);

  it("should close the store while a receive is in flight", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    // starts the receiver thread, so the next request only has to wake it
    await core.chatRecvMsgWait(ctrl, 1);
    const settle = (p: Promise<unknown>) => p.then((event) => ({event}), (e: Error) => ({error: e.message}));
    const receives = Promise.all([settle(core.chatRecvMsgWait(ctrl, 3_000_000)), settle(core.chatRecvMsgWait(ctrl, 3_000_000))]);
    // the thread enters the first receive within this margin even under load, so the second one is still queued at close
    await new Promise((resolve) => setTimeout(resolve, 500));
    await expect(core.chatSendCmd(ctrl, "/_stop")).resolves.toMatchObject({type: "chatStopped"});
    let closed = false;
    const close = core.chatCloseStore(ctrl).then(() => { closed = true; });
    const timerStart = Date.now();
    const timerDelay = await new Promise<number>((resolve) => setTimeout(() => resolve(Date.now() - timerStart), 10));
    expect({closed, timerDelayBelow100ms: timerDelay < 100}).toEqual({closed: false, timerDelayBelow100ms: true});
    await close;
    expect(await receives).toEqual([{event: undefined}, {error: "chat receiver stopped"}]);
  }, 10000);

  it("should let the process exit while a receiver is idle", async () => {
    const childDbPath = path.resolve(tmpDir, "simplex_child");
    const libPath = path.resolve(libs.libPath(await libs.resolveLibsDir("sqlite")));
    const script = `
      const simplex = require("./build/Release/simplex.node");
      simplex.load(${JSON.stringify(libPath)});
      simplex.chat_migrate_init(${JSON.stringify(childDbPath)}, "key", "yesUp")
        .then(([ctrl]) => simplex.chat_recv_msg_wait(ctrl, 1))
        .then((res) => console.log("received " + JSON.stringify(res)));
    `;
    const child = spawnSync(process.execPath, ["-e", script], {cwd: path.join(__dirname, ".."), timeout: 10000, encoding: "utf8"});
    if (child.status !== 0 || child.signal !== null) console.log("child stderr:", child.stderr);
    expect({status: child.status, signal: child.signal, stdout: child.stdout.trim()})
      .toEqual({status: 0, signal: null, stdout: 'received ""'});
  }, 15000);

  it("should not crash when closing stopped controllers repeatedly", async () => {
    const libPath = path.resolve(libs.libPath(await libs.resolveLibsDir("sqlite")));
    const script = `
      const fs = require("fs"), path = require("path");
      const simplex = require("./build/Release/simplex.node");
      simplex.load(${JSON.stringify(libPath)});
      (async () => {
        for (let i = 0; i < 40; i++) {
          const dir = fs.mkdtempSync(path.join(${JSON.stringify(path.resolve(tmpDir))}, "close-"));
          const [ctrl] = await simplex.chat_migrate_init(path.join(dir, "simplex"), "key", "yesUp");
          await simplex.chat_send_cmd(ctrl, "/v");
          await simplex.chat_send_cmd(ctrl, "/_stop");
          const res = await simplex.chat_close_store(ctrl);
          fs.rmSync(dir, {recursive: true, force: true});
          if (res !== "") throw new Error("close failed: " + res);
        }
      })();
    `;
    const runChild = () => new Promise<{code: number | null, signal: NodeJS.Signals | null, stderr: string}>((resolve) => {
      const child = execFile(process.execPath, ["-e", script], {cwd: path.join(__dirname, ".."), timeout: 150000}, (_error, _stdout, stderr) =>
        resolve({code: child.exitCode, signal: child.signalCode, stderr}));
    });
    const childCount = 3;
    const results = await Promise.all(Array.from({length: childCount}, runChild));
    for (const r of results) if (r.code !== 0 || r.signal !== null) console.log("child stderr:", r.stderr);
    expect(results.map(({code, signal}) => ({code, signal}))).toEqual(Array(childCount).fill({code: 0, signal: null}));
  }, 180000);
});
