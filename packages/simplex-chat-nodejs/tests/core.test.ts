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
});
