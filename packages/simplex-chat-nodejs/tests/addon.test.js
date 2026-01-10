const core = require('../src/index');
const fs = require('fs');
const path = require('path');

describe('Addon Tests', () => {
  const tmpDir = './tests/tmp';
  const dbPath = path.join(tmpDir, 'simplex_v1');

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}));

  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}));

  it('should initialize controller', async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", "yesUp");
    expect(typeof ctrl).toBe("bigint");
    await expect(core.chatCloseStore(ctrl)).resolves.toBe(undefined);
    
    await expect(core.chatMigrateInit(dbPath, "wrong_key", "yesUp")).rejects.toMatchObject({
      message: 'Database or migration error (see dbMigrationError property)',
      dbMigrationError: expect.objectContaining({type: 'errorNotADatabase'})
    });
    await expect(core.chatMigrateInit(dbPath, "key", "invalidMigrationMode")).rejects.toMatchObject({
      message: 'Database or migration error (see dbMigrationError property)',
      dbMigrationError: expect.objectContaining({type: 'invalidConfirmation'})
    });
  });
  
  it('should send command and receive event', async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", "yesUp");

    await expect(core.chatSendCmd(ctrl, "/v")).resolves.toHaveProperty('result');
    await expect(core.chatSendCmd(ctrl, '/debug event {"type": "chatSuspended"}')).resolves.toMatchObject({result: {type: 'cmdOk'}});

    const wait = 500_000;
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toMatchObject({result: {type: 'chatSuspended'}});    
    await expect(core.chatRecvMsgWait(ctrl, wait)).resolves.toBe(undefined);
    
    await expect(core.chatSendCmd(ctrl, '/unknown')).resolves.toHaveProperty('error');

    await core.chatCloseStore(ctrl);
  });

  it('should write/read encrypted file from/to buffer', async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", "yesUp");

    const filePath = path.join(tmpDir, 'write_file.txt');
    const buffer = new Uint8Array([0, 1, 2]).buffer;
    const encRes = await core.chatWriteFile(ctrl, filePath, buffer);
    const key = encRes.fileKey;
    const nonce = encRes.fileNonce;
    expect(typeof key).toBe('string');
    expect(typeof nonce).toBe('string');

    const buffer2 = await core.chatReadFile(filePath, key, nonce);
    expect(Buffer.from(buffer2).equals(Buffer.from(buffer))).toBe(true);

    await expect(core.chatWriteFile(ctrl, path.join(tmpDir, 'unknown', 'unknown.txt'), buffer)).rejects.toThrow();
    await expect(core.chatReadFile(path.join(tmpDir, 'unknown.txt'), key, nonce)).rejects.toThrow();
    
    core.chatCloseStore(ctrl);
  });

  it('should encrypt/decrypt file', async () => {
    const ctrl = await core.chatMigrateInit(dbPath, "key", "yesUp");

    const unencryptedPath = path.join(tmpDir, 'file_unencrypted.txt');
    fs.writeFileSync(unencryptedPath, "unencrypted\n");
    const encryptedPath = path.join(tmpDir, 'file_encrypted.txt');
    const encRes = await core.chatEncryptFile(ctrl, unencryptedPath, encryptedPath);
    const key = encRes.fileKey;
    const nonce = encRes.fileNonce;
    expect(typeof key).toBe('string');
    expect(typeof nonce).toBe('string');

    const decryptedPath = path.join(tmpDir, 'file_decrypted.txt');
    await expect(core.chatDecryptFile(encryptedPath, key, nonce, decryptedPath)).resolves.toBe(undefined);

    expect(fs.readFileSync(decryptedPath, 'utf8')).toBe("unencrypted\n");

    await expect(core.chatEncryptFile(ctrl, path.join(tmpDir, 'unknown.txt'), encryptedPath)).rejects.toThrow();
    await expect(core.chatDecryptFile(path.join(tmpDir, 'unknown.txt'), key, nonce, decryptedPath)).rejects.toThrow();
    
    core.chatCloseStore(ctrl);
  });
});


// data DBMigrationResult
//   = DBMOk
//   | DBMInvalidConfirmation
//   | DBMErrorNotADatabase {dbFile :: String}
//   | DBMErrorMigration {dbFile :: String, migrationError :: MigrationError}
//   | DBMErrorSQL {dbFile :: String, migrationSQLError :: String}