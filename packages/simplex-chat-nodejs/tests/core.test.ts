import {spawnSync} from "child_process";
import * as fs from "fs";
import * as path from "path";
import {core} from "../src/index";

describe("Core tests", () => {
  const tmpDir = "./tests/tmp";
  const dbPath = path.join(tmpDir, "simplex_v1");

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}));
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}));

  it("should initialize chat controller", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    expect(typeof ctrl).toBe("bigint");
    await expect(core.chatCloseStore(ctrl)).resolves.toBe(undefined);
    
    await expect(core.chatMigrateInit(dbPath, "wrong_key", core.MigrationConfirmation.YesUp)).rejects.toMatchObject({
      message: "Database or migration error (see dbMigrationError property)",
      dbMigrationError: expect.objectContaining({type: "errorNotADatabase"})
    });
  });

  it("should initialize chat controller with queue size", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp, 65536);
    expect(typeof ctrl).toBe("bigint");
    await expect(core.chatCloseStore(ctrl)).resolves.toBe(undefined);

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

    await core.chatCloseStore(ctrl);
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
    
    await core.chatCloseStore(ctrl);
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

    await core.chatCloseStore(ctrl);
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
    
    await core.chatCloseStore(ctrl);
  });

  it("should not block the libuv pool while receiving", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    const receives = [1, 2, 3, 4].map(() => core.chatRecvMsgWait(ctrl, 2_000_000));
    const start = Date.now();
    await fs.promises.stat(tmpDir);
    expect(Date.now() - start).toBeLessThan(200);
    await Promise.all(receives);
    await core.chatCloseStore(ctrl);
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
    await core.chatCloseStore(ctrl);
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
    await core.chatCloseStore(ctrlA);
    await core.chatCloseStore(ctrlB);
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
    await core.chatCloseStore(ctrl);
  }, 10000);

  it("should close the store while a receive is in flight", async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", core.MigrationConfirmation.YesUp);
    const settle = (p: Promise<unknown>) => p.then((event) => ({event}), (e: Error) => ({error: e.message}));
    const receives = Promise.all([settle(core.chatRecvMsgWait(ctrl, 2_000_000)), settle(core.chatRecvMsgWait(ctrl, 2_000_000))]);
    // lets the receiver thread enter the first receive, so the second one is still queued at close
    await new Promise((resolve) => setTimeout(resolve, 50));
    let closed = false;
    const close = core.chatCloseStore(ctrl).then(() => { closed = true; });
    const timerStart = Date.now();
    const timerDelay = await new Promise<number>((resolve) => setTimeout(() => resolve(Date.now() - timerStart), 10));
    expect({closed, timerDelayBelow100ms: timerDelay < 100}).toEqual({closed: false, timerDelayBelow100ms: true});
    await close;
    expect(await receives).toEqual([{event: undefined}, {error: "chat receiver stopped"}]);
  }, 10000);

  it("should let the process exit while a receiver is idle", () => {
    const childDbPath = path.resolve(tmpDir, "simplex_child");
    const script = `
      const simplex = require("./build/Release/simplex.node");
      simplex.chat_migrate_init(${JSON.stringify(childDbPath)}, "key", "yesUp")
        .then(([ctrl]) => simplex.chat_recv_msg_wait(ctrl, 1))
        .then((res) => console.log("received " + JSON.stringify(res)));
    `;
    const child = spawnSync(process.execPath, ["-e", script], {cwd: path.join(__dirname, ".."), timeout: 10000, encoding: "utf8"});
    expect({status: child.status, signal: child.signal, stdout: child.stdout.trim(), stderr: child.stderr})
      .toEqual({status: 0, signal: null, stdout: 'received ""', stderr: ""});
  }, 15000);
});
