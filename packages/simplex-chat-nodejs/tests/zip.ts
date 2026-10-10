import * as zlib from "zlib"

const LOCAL_FILE_HEADER = 0x04034b50
const CENTRAL_DIRECTORY_HEADER = 0x02014b50
const END_OF_CENTRAL_DIRECTORY = 0x06054b50
const ZIP_VERSION = 20
const LOCAL_HEADER_SIZE = 30
const CENTRAL_ENTRY_SIZE = 46
const END_RECORD_SIZE = 22

export function storedZip(files: {[name: string]: string}): Buffer {
  const local: Buffer[] = []
  const central: Buffer[] = []
  let offset = 0
  for (const [name, content] of Object.entries(files)) {
    const nameBuf = Buffer.from(name)
    const data = Buffer.from(content)
    const crc = zlib.crc32(data)
    const header = Buffer.alloc(LOCAL_HEADER_SIZE)
    header.writeUInt32LE(LOCAL_FILE_HEADER, 0)
    header.writeUInt16LE(ZIP_VERSION, 4)
    header.writeUInt32LE(crc, 14)
    header.writeUInt32LE(data.length, 18)
    header.writeUInt32LE(data.length, 22)
    header.writeUInt16LE(nameBuf.length, 26)
    local.push(header, nameBuf, data)
    const entry = Buffer.alloc(CENTRAL_ENTRY_SIZE)
    entry.writeUInt32LE(CENTRAL_DIRECTORY_HEADER, 0)
    entry.writeUInt16LE(ZIP_VERSION, 4)
    entry.writeUInt16LE(ZIP_VERSION, 6)
    entry.writeUInt32LE(crc, 16)
    entry.writeUInt32LE(data.length, 20)
    entry.writeUInt32LE(data.length, 24)
    entry.writeUInt16LE(nameBuf.length, 28)
    entry.writeUInt32LE(offset, 42)
    central.push(entry, nameBuf)
    offset += header.length + nameBuf.length + data.length
  }
  const centralBuf = Buffer.concat(central)
  const end = Buffer.alloc(END_RECORD_SIZE)
  end.writeUInt32LE(END_OF_CENTRAL_DIRECTORY, 0)
  end.writeUInt16LE(Object.keys(files).length, 8)
  end.writeUInt16LE(Object.keys(files).length, 10)
  end.writeUInt32LE(centralBuf.length, 12)
  end.writeUInt32LE(offset, 16)
  return Buffer.concat([...local, centralBuf, end])
}
