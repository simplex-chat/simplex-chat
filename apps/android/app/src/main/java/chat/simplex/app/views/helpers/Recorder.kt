package chat.simplex.app.views.helpers

import android.media.*
import android.os.Build
import android.util.Log
import androidx.compose.runtime.MutableState
import chat.simplex.app.SimplexApp
import chat.simplex.app.TAG
import cn.org.hentai.acodec.AudioCodec
import cn.org.hentai.acodec.CodecFactory
import kotlinx.coroutines.*
import java.io.*
import java.nio.ByteBuffer

interface Recorder {
  fun start(recordingInProgress: MutableState<Boolean>): String
  fun stop(recordingInProgress: MutableState<Boolean>)
  fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>)
}

class RecorderNative: Recorder {
  private val recorder =
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      MediaRecorder(SimplexApp.context)
    } else {
      MediaRecorder()
    }

  override fun start(recordingInProgress: MutableState<Boolean>): String {
    recordingInProgress.value = true
    recorder.setAudioSource(MediaRecorder.AudioSource.MIC)
    recorder.setOutputFormat(MediaRecorder.OutputFormat.AMR_WB)
    recorder.setAudioEncoder(MediaRecorder.AudioEncoder.AMR_WB)
    recorder.setAudioChannels(1)
    recorder.setAudioSamplingRate(8000)
    recorder.setAudioEncodingBitRate(8000)
    recorder.setMaxDuration(-1)
    val filePath = getAppFilePath(SimplexApp.context, uniqueCombine(SimplexApp.context, getAppFilePath(SimplexApp.context, "voice.amr")))
    recorder.setOutputFile(filePath)
    recorder.prepare()
    recorder.start()
    return filePath
  }

  override fun stop(recordingInProgress: MutableState<Boolean>) {
    recordingInProgress.value = false
    runCatching {
      recorder.stop()
    }
    runCatching {
      recorder.reset()
    }
  }

  override fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>) {
    stop(recordingInProgress)
    runCatching { File(filePath).delete() }.getOrElse { Log.d(TAG, "Unable to delete a file: ${it.stackTraceToString()}") }
  }
}

class RecorderExternal: Recorder {
  companion object {
    private const val SAMPLE_RATE = 8000
    private const val ENCODING = AudioFormat.ENCODING_PCM_16BIT
    private val BUFFER_SIZE = AudioRecord.getMinBufferSize(SAMPLE_RATE, AudioFormat.CHANNEL_IN_MONO, ENCODING) * 2
  }

  private val recorder = AudioRecord(
    MediaRecorder.AudioSource.DEFAULT, SAMPLE_RATE, AudioFormat.CHANNEL_IN_MONO, ENCODING, BUFFER_SIZE
  )

  private var job: Job? = null

  override fun start(recordingInProgress: MutableState<Boolean>): String {
    recordingInProgress.value = true
    val filePath = getAppFilePath(SimplexApp.context, uniqueCombine(SimplexApp.context, getAppFilePath(SimplexApp.context, "voice.pcm")))
    recorder.startRecording()
    job = CoroutineScope(Dispatchers.Default).launch {
      val file = File(filePath)
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(BUFFER_SIZE)


      try {
        withContext(Dispatchers.IO) {
          FileOutputStream(file).use { outStream ->
            while (recordingInProgress.value) {
              val result: Int = recorder.read(buffer, BUFFER_SIZE)
              if (result < 0) {
                Log.d(TAG, "Can't read audio buffer: " + errorCodeToString(result))
                break
              }
              outStream.write(buffer.array(), 0, BUFFER_SIZE)
              buffer.clear()
            }
            encode(filePath)
          }
        }
      } catch (e: IOException) {
        throw RuntimeException("Can't write audio buffer", e)
      }
    }
    return filePath
  }

  override fun stop(recordingInProgress: MutableState<Boolean>) {
    runCatching {
      recorder.stop()
    }
    recordingInProgress.value = false
    //    job?.cancel()
  }

  override fun cancel(filePath: String, recordingInProgress: MutableState<Boolean>) {
    stop(recordingInProgress)
    runCatching { File(filePath).delete() }.getOrElse { Log.d(TAG, "Unable to delete a file: ${it.stackTraceToString()}") }
  }

  private fun encode(filePath: String) {
    val codec: AudioCodec = CodecFactory.getCodec("g711a")
    val pcmData: ByteArray = File(filePath).readBytes()

    val audio = AudioTrack(
      AudioManager.STREAM_MUSIC,
      SAMPLE_RATE,
      AudioFormat.CHANNEL_OUT_MONO,
      AudioFormat.ENCODING_PCM_16BIT,
      BUFFER_SIZE,
      AudioTrack.MODE_STREAM )
    audio.play()
    audio.write(pcmData, 0, pcmData.size)

    val res = codec.fromPCM(pcmData)
    File(filePath.replace(".pcm", ".g711pcm")).outputStream().use { it.write(res) }
  }

  private fun errorCodeToString(errorCode: Int): String {
    return when (errorCode) {
      AudioRecord.ERROR_INVALID_OPERATION -> "ERROR_INVALID_OPERATION"
      AudioRecord.ERROR_BAD_VALUE -> "ERROR_BAD_VALUE"
      AudioRecord.ERROR_DEAD_OBJECT -> "ERROR_DEAD_OBJECT"
      AudioRecord.ERROR -> "ERROR"
      else -> "Unknown ($errorCode)"
    }
  }
}