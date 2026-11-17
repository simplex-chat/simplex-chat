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
external fun chatSendCmdRetry(ctrl: ChatCtrl, msg: String, retryNum: Int): String
external fun chatSendRemoteCmdRetry(ctrl: ChatCtrl, rhId: Int, msg: String, retryNum: Int): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
external fun chatParseUri(str: String, safe: Int): String
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

private val chatEvents = LinkedBlockingQueue<String>()
@Volatile private var chatEventsCtrl: ChatCtrl? = null

fun takeChatEvent(ctrl: ChatCtrl, timeout: Int): String {
  synchronized(chatEvents) {
    if (chatEventsCtrl != ctrl) {
      chatEventsCtrl = ctrl
      chatEvents.clear()
      val reader = Thread({
        while (chatEventsCtrl == ctrl) {
          val event = chatRecvMsgWait(ctrl, timeout)
          if (event != "" && chatEventsCtrl == ctrl) chatEvents.put(event)
        }
      }, "simplex-core-events")
      reader.isDaemon = true
      reader.start()
    }
  }
  return chatEvents.poll(timeout.toLong(), TimeUnit.MICROSECONDS) ?: ""
}
