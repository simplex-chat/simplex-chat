import * as fs from "fs";
import * as path from "path";
import {core} from "../src/index";
import {MigrationConfirmation} from "../src/types";

describe("Core tests", () => {
  const tmpDir: string = "./tests/tmp";
  const dbPath: string = path.join(tmpDir, "simplex_v1");

  beforeEach(() => fs.mkdirSync(tmpDir, { recursive: true }));

  afterEach(() => fs.rmSync(tmpDir, { recursive: true, force: true }));

  it("should initialize chat controller", async () => {
    const ctrl: bigint = await core.chatMigrateInit(dbPath, "key", MigrationConfirmation.YesUp);
    expect(typeof ctrl).toBe("bigint");
    await expect(core.chatCloseStore(ctrl)).resolves.toBe(undefined);
    
    await expect(core.chatMigrateInit(dbPath, "wrong_key", MigrationConfirmation.YesUp)).rejects.toMatchObject({
      message: "Database or migration error (see dbMigrationError property)",
      dbMigrationError: expect.objectContaining({ type: "errorNotADatabase" })
    });
  });
  
  it("should send command and receive event", async () => {
    const ctrl: bigint = await core.chatMigrateInit(dbPath, "key", MigrationConfirmation.YesUp);

    await expect(core.chatSendCmd(ctrl, "/v")).resolves.toHaveProperty("result");
    await expect(core.chatSendCmd(ctrl, '/debug event {"type": "chatSuspended"}')).resolves.toMatchObject({ result: { type: "cmdOk" } });

    const wait: number = 500_000;
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toMatchObject({ result: { type: "chatSuspended" } });    
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toBe(undefined);
    
    await expect(core.chatSendCmd(ctrl, "/unknown")).resolves.toHaveProperty("error");

    await core.chatCloseStore(ctrl);
  });

  it("should write/read encrypted file from/to buffer", async () => {
    const ctrl: bigint = await core.chatMigrateInit(dbPath, "key", MigrationConfirmation.YesUp);

    const filePath: string = path.join(tmpDir, "write_file.txt");
    const buffer: ArrayBuffer = new Uint8Array([0, 1, 2]).buffer;
    const encRes: { fileKey: string; fileNonce: string } = await core.chatWriteFile(ctrl, filePath, buffer);
    const key: string = encRes.fileKey;
    const nonce: string = encRes.fileNonce;
    expect(typeof key).toBe("string");
    expect(typeof nonce).toBe("string");

    const buffer2: ArrayBuffer = await core.chatReadFile(filePath, key, nonce);
    expect(Buffer.from(buffer2).equals(Buffer.from(buffer))).toBe(true);

    await expect(core.chatWriteFile(ctrl, path.join(tmpDir, "unknown", "unknown.txt"), buffer)).rejects.toThrow();
    await expect(core.chatReadFile(path.join(tmpDir, "unknown.txt"), key, nonce)).rejects.toThrow();
    
    await core.chatCloseStore(ctrl);
  });

  it("should encrypt/decrypt file", async () => {
    const ctrl: bigint = await core.chatMigrateInit(dbPath, "key", MigrationConfirmation.YesUp);

    const unencryptedPath: string = path.join(tmpDir, "file_unencrypted.txt");
    fs.writeFileSync(unencryptedPath, "unencrypted\n");
    const encryptedPath: string = path.join(tmpDir, "file_encrypted.txt");
    const encRes: { fileKey: string; fileNonce: string } = await core.chatEncryptFile(ctrl, unencryptedPath, encryptedPath);
    const key: string = encRes.fileKey;
    const nonce: string = encRes.fileNonce;
    expect(typeof key).toBe("string");
    expect(typeof nonce).toBe("string");

    const decryptedPath: string = path.join(tmpDir, "file_decrypted.txt");
    await expect(core.chatDecryptFile(encryptedPath, key, nonce, decryptedPath)).resolves.toBe(undefined);

    expect(fs.readFileSync(decryptedPath, "utf8")).toBe("unencrypted\n");

    await expect(core.chatEncryptFile(ctrl, path.join(tmpDir, "unknown.txt"), encryptedPath)).rejects.toThrow();
    await expect(core.chatDecryptFile(path.join(tmpDir, "unknown.txt"), key, nonce, decryptedPath)).rejects.toThrow();
    
    await core.chatCloseStore(ctrl);
  });
});
