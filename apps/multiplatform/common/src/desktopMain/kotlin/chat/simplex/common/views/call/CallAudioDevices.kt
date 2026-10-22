package chat.simplex.common.views.call

import dev.onvoid.webrtc.media.audio.AudioDevice
import kotlinx.serialization.Serializable

@Serializable
data class SavedAudioDevice(val id: String, val name: String)

data class CallAudioSettings(val mic: SavedAudioDevice?, val speaker: SavedAudioDevice?, val fallback: Boolean)

enum class AudioDeviceKind { Mic, Speaker }

enum class AudioDeviceState { Connected, Fallback, Unavailable }

data class AudioDeviceNotice(val kind: AudioDeviceKind, val deviceName: String, val state: AudioDeviceState)

fun savedAudioDevice(device: AudioDevice): SavedAudioDevice = SavedAudioDevice(device.descriptor, device.name)

private class AbsentAudioDevice(saved: SavedAudioDevice) : AudioDevice(saved.name, saved.id)

// The saved device stays chosen while it is absent and is selected again once it is listed
class AudioDeviceChoice(
  val kind: AudioDeviceKind,
  private var saved: SavedAudioDevice?,
  private val listDevices: () -> List<AudioDevice>,
  private val deviceInUse: () -> AudioDevice?,
  private val selectDevice: (AudioDevice) -> Unit,
  private val selectDefault: () -> Unit,
) {
  private var selectedId: String? = null
  private var state = AudioDeviceState.Connected

  // The system default entry has no descriptor
  fun devices(): List<AudioDevice> = listDevices().filter { it.descriptor != null }

  fun choose(device: AudioDevice?) {
    if (device == null) selectDefault() else selectDevice(device)
    saved = device?.let(::savedAudioDevice)
    selectedId = device?.descriptor
    state = AudioDeviceState.Connected
  }

  fun update(): AudioDeviceNotice? {
    val saved = saved ?: return null
    val devices = devices()
    val device = devices.firstOrNull { it.descriptor == saved.id } ?: devices.singleOrNull { it.name == saved.name }
    val id = device?.descriptor ?: saved.id
    if (id != selectedId) {
      selectDevice(device ?: AbsentAudioDevice(saved))
      selectedId = id
    }
    val inUse = deviceInUse()
    val newState = when {
      device != null || inUse?.descriptor == id -> AudioDeviceState.Connected
      inUse == null -> AudioDeviceState.Unavailable
      else -> AudioDeviceState.Fallback
    }
    if (newState == state) return null
    state = newState
    return AudioDeviceNotice(kind, saved.name, newState)
  }
}
