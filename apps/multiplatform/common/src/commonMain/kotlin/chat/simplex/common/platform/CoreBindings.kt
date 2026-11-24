@file:JvmName("CoreKt")

package chat.simplex.common.platform

import java.nio.ByteBuffer
import java.nio.channels.FileLock
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit

// ghc's rts
// Spec: spec/architecture.md#initHS
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
// Spec: spec/architecture.md#chatMigrateInit
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatCloseStore(ctrl: ChatCtrl): String
@JvmName("chatSendCmdRetry") external fun nativeChatSendCmdRetry(ctrl: ChatCtrl, msg: String, retryNum: Int): String
external fun chatSendRemoteCmdRetry(ctrl: ChatCtrl, rhId: Int, msg: String, retryNum: Int): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
@JvmName("chatParseMarkdown") external fun nativeChatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
@JvmName("chatParseUri") external fun nativeChatParseUri(str: String, safe: Int): String
external fun chatPasswordHash(pwd: String, salt: String): String
external fun chatValidName(name: String): String
external fun chatParseBadgeCode(code: String): String
external fun chatJsonLength(str: String): Int
external fun chatWriteFile(ctrl: ChatCtrl, path: String, buffer: ByteBuffer): String
external fun chatReadFile(path: String, key: String, nonce: String): Array<Any>
external fun chatEncryptFile(ctrl: ChatCtrl, fromPath: String, toPath: String): String
external fun chatDecryptFile(fromPath: String, key: String, nonce: String, toPath: String): String
external fun chatResizeImageUri(buffer: ByteBuffer, maxSize: Int): String
external fun chatResizeImage(buffer: ByteBuffer, maxSize: Int, png: Int): String
external fun chatResizeImageFileUri(path: String, maxSize: Int): String
external fun chatResizeImageFile(path: String, maxSize: Int, png: Int): String
external fun chatResizeImageFdUri(fd: Int, maxSize: Int): String
external fun chatResizeImageFd(fd: Int, maxSize: Int, png: Int): String

@Suppress("UnsafeDynamicallyLoadedCode")
fun loadCoreLibrary(path: String) = System.load(path)

var reloadedChatCtrl: ChatCtrl? = null
var reloadedChatDbEncrypted: Boolean? = null
var reloadedInstanceLock: FileLock? = null
var uiReloadRequested = false

private class ChatEvents(val ctrl: ChatCtrl) {
  val queue = LinkedBlockingQueue<String>(1)
}

private val chatEventsLock = Any()
@Volatile private var chatEvents: ChatEvents? = null

fun takeChatEvent(ctrl: ChatCtrl, timeout: Int): String {
  val events = synchronized(chatEventsLock) {
    chatEvents?.takeIf { it.ctrl == ctrl } ?: ChatEvents(ctrl).also { created ->
      chatEvents = created
      val reader = Thread({
        while (chatEvents === created) {
          val event = chatRecvMsgWait(ctrl, timeout)
          while (event != "" && chatEvents === created && !created.queue.offer(event, 1, TimeUnit.SECONDS)) {}
        }
      }, "simplex-core-events")
      reader.isDaemon = true
      reader.start()
    }
  }
  return events.queue.poll(timeout.toLong(), TimeUnit.MICROSECONDS) ?: ""
}
