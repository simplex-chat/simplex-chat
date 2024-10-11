package chat.simplex.common.views.call

import android.content.Context
import android.media.*
import android.media.AudioManager.OnCommunicationDeviceChangedListener
import android.os.Build
import androidx.annotation.RequiresApi
import androidx.compose.runtime.*
import chat.simplex.common.platform.*
import chat.simplex.common.views.helpers.withBGApi
import dev.icerock.moko.resources.ImageResource
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import java.util.concurrent.Executors

interface CallAudioDeviceManagerInterface {
  val devices: State<List<AudioDeviceInfo>>
  val currentDevice: MutableState<AudioDeviceInfo?>
  fun start()
  fun stop()
  // AudioDeviceInfo.AudioDeviceType
  fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyExternal: Boolean)
  fun selectSameDeviceOnWebViewChange()
  // AudioDeviceInfo.AudioDeviceType
  fun selectDevice(id: Int)

  companion object {
    fun new(): CallAudioDeviceManagerInterface =
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
        PostSCallAudioDeviceManager()
      } else {
        PreSCallAudioDeviceManager()
      }
  }
}

@RequiresApi(Build.VERSION_CODES.S)
class PostSCallAudioDeviceManager: CallAudioDeviceManagerInterface {
  private val am = androidAppContext.getSystemService(Context.AUDIO_SERVICE) as AudioManager
  private var resetDeviceToDevice: AudioDeviceInfo? = null
  private var resetDeviceToDeviceJob: Job = Job()
  override val devices: MutableState<List<AudioDeviceInfo>> = mutableStateOf(emptyList())
  override val currentDevice: MutableState<AudioDeviceInfo?> = mutableStateOf(null)

  private val audioCallback = object: AudioDeviceCallback() {
    override fun onAudioDevicesAdded(addedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Added audio devices: ${addedDevices.map { it.type }}")
      resetDeviceToDevice = null
      super.onAudioDevicesAdded(addedDevices)
      val oldDevices = devices.value
      devices.value = am.availableCommunicationDevices
      Log.d(TAG, "Added audio devices2: ${devices.value.map { it.type }}")

      if (devices.value.size - oldDevices.size > 0) {
        selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.hasVideo == true, false)
      }
    }

    override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Removed audio devices: ${removedDevices.map { it.type }}")
      resetDeviceToDevice = null
      super.onAudioDevicesRemoved(removedDevices)
      devices.value = am.availableCommunicationDevices
    }
  }

  private val listener: OnCommunicationDeviceChangedListener = OnCommunicationDeviceChangedListener { device ->
    val resetTo = resetDeviceToDevice
    if (resetTo != null && device != null && resetTo.id != device.id) {
      Log.w(TAG, "Resetting device that was set by WebView ${device.name?.localized()} to previously set device ${resetTo.name?.localized()}")
      selectDevice(resetTo.id)
      return@OnCommunicationDeviceChangedListener
    }
    devices.value = am.availableCommunicationDevices
    //Log.d(TAG, "Devices changed ${device?.details()} | ${devices.value.map { it.details() }}")
    currentDevice.value = device
  }

  override fun start() {
    am.mode = AudioManager.MODE_IN_COMMUNICATION
    currentDevice.value = am.communicationDevice
    am.registerAudioDeviceCallback(audioCallback, null)
    am.addOnCommunicationDeviceChangedListener(Executors.newSingleThreadExecutor(), listener)
  }

  override fun stop() {
    am.unregisterAudioDeviceCallback(audioCallback)
    am.removeOnCommunicationDeviceChangedListener(listener)
  }

  override fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyExternal: Boolean) {
    Log.d(TAG, "selectLastExternalDeviceOrDefault: set audio mode, speaker enabled: $speaker")
    val commDevice = am.communicationDevice
    if (keepAnyExternal && commDevice != null && commDevice.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE && commDevice.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER) {
      // some external device selected already, no need to change it
      return
    }

    val preferredInternalDevice = if (speaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    val externalDevice = devices.value.lastOrNull { it.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE && it.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER }
    // External device already selected
    if (externalDevice != null && externalDevice.type == am.communicationDevice?.type) {
      return
    }
    if (externalDevice != null) {
      am.setCommunicationDevice(externalDevice)
    } else if (am.communicationDevice?.type != preferredInternalDevice) {
      am.availableCommunicationDevices.firstOrNull { it.type == preferredInternalDevice }?.let {
        am.setCommunicationDevice(it)
      }
    }
  }

  // WebView modifies speaker when muting/unmuting microphone. It's not needed at all, returning back current device if it will be changed
  override fun selectSameDeviceOnWebViewChange() {
    resetDeviceToDevice = currentDevice.value
    resetDeviceToDeviceJob.cancel()
    resetDeviceToDeviceJob = withBGApi {
      delay(5000)
      resetDeviceToDevice = null
    }
  }

  override fun selectDevice(id: Int) {
    resetDeviceToDevice = null
    val device = devices.value.lastOrNull { it.id == id }
    if (device != null && am.communicationDevice?.id != id ) {
      am.setCommunicationDevice(device)
    }
  }
}

class PreSCallAudioDeviceManager: CallAudioDeviceManagerInterface {
  private val am = androidAppContext.getSystemService(Context.AUDIO_SERVICE) as AudioManager
  override val devices: MutableState<List<AudioDeviceInfo>> = mutableStateOf(emptyList())
  override val currentDevice: MutableState<AudioDeviceInfo?> = mutableStateOf(null)

  private val audioCallback = object: AudioDeviceCallback() {
    override fun onAudioDevicesAdded(addedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Added audio devices: ${addedDevices.map { it.type }}")
      super.onAudioDevicesAdded(addedDevices)
      devices.value = am.getDevices(AudioManager.GET_DEVICES_OUTPUTS).filter { it.hasSupportedType() }.excludeSameType().excludeEarpieceIfWired()
      selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.hasVideo == true, false)
    }

    override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Removed audio devices: ${removedDevices.map { it.type }}")
      super.onAudioDevicesRemoved(removedDevices)
      devices.value = am.getDevices(AudioManager.GET_DEVICES_OUTPUTS).filter { it.hasSupportedType() }.excludeSameType().excludeEarpieceIfWired()
      selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.hasVideo == true, true)
    }
  }

  override fun start() {
    am.mode = AudioManager.MODE_IN_COMMUNICATION
    am.registerAudioDeviceCallback(audioCallback, null)
  }

  override fun stop() {
    am.unregisterAudioDeviceCallback(audioCallback)
    am.stopBluetoothSco()
  }

  override fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyExternal: Boolean) {
    Log.d(TAG, "selectLastExternalDeviceOrDefault: set audio mode, speaker enabled: $speaker")
    val preferredInternalDevice = if (speaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    val externalDevice = devices.value.lastOrNull { it.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE && it.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER }
    if (externalDevice != null) {
      selectDevice(externalDevice.id)
    } else {
      am.stopBluetoothSco()
      am.isWiredHeadsetOn = false
      am.isSpeakerphoneOn = preferredInternalDevice == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
      am.isBluetoothScoOn = false
      val newCurrentDevice = devices.value.firstOrNull { it.type == preferredInternalDevice }
      adaptToCurrentlyActiveDevice(newCurrentDevice)
    }
  }

  // Works without it
  override fun selectSameDeviceOnWebViewChange() {}

  override fun selectDevice(id: Int) {
    val device = devices.value.lastOrNull { it.id == id }
    val isExternalDevice = device != null && device.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE && device.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
    if (isExternalDevice) {
      am.isSpeakerphoneOn = false
      if (device?.type == AudioDeviceInfo.TYPE_WIRED_HEADSET || device?.type == AudioDeviceInfo.TYPE_WIRED_HEADPHONES) {
        am.isWiredHeadsetOn = true
        am.stopBluetoothSco()
        am.isBluetoothScoOn = false
      } else {
        am.isWiredHeadsetOn = false
        am.startBluetoothSco()
        am.isBluetoothScoOn = true
      }
      adaptToCurrentlyActiveDevice(device)
    } else {
      am.stopBluetoothSco()
      am.isWiredHeadsetOn = false
      am.isSpeakerphoneOn = device?.type == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
      am.isBluetoothScoOn = false
      adaptToCurrentlyActiveDevice(device)
    }
  }

  private fun adaptToCurrentlyActiveDevice(newCurrentDevice: AudioDeviceInfo?) {
    currentDevice.value = newCurrentDevice
  }

  private fun AudioDeviceInfo.hasSupportedType(): Boolean = when (type) {
    AudioDeviceInfo.TYPE_BUILTIN_EARPIECE -> true
    AudioDeviceInfo.TYPE_BUILTIN_SPEAKER -> true

    AudioDeviceInfo.TYPE_BLUETOOTH_SCO -> true
    AudioDeviceInfo.TYPE_WIRED_HEADPHONES -> true
    else -> false
  }

  private fun List<AudioDeviceInfo>.excludeSameType(): List<AudioDeviceInfo> =
    groupBy { it.type }.flatMap { devices -> listOf(devices.value.minByOrNull { it.id }!!) }

  // Earpiece will not work if there is a wired connection
  private fun List<AudioDeviceInfo>.excludeEarpieceIfWired(): List<AudioDeviceInfo> =
    if (any { it.type == AudioDeviceInfo.TYPE_WIRED_HEADSET || it.type == AudioDeviceInfo.TYPE_WIRED_HEADPHONES })
      filter { it.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE }
    else this
}

val AudioDeviceInfo.icon: ImageResource
  get() = when (this.type) {
    AudioDeviceInfo.TYPE_BUILTIN_EARPIECE -> MR.images.ic_volume_down
    AudioDeviceInfo.TYPE_BUILTIN_SPEAKER -> MR.images.ic_volume_up

    AudioDeviceInfo.TYPE_BLUETOOTH_SCO,
    AudioDeviceInfo.TYPE_BLUETOOTH_A2DP,
    AudioDeviceInfo.TYPE_BLE_HEADSET,
    AudioDeviceInfo.TYPE_BLE_SPEAKER -> MR.images.ic_bluetooth

    AudioDeviceInfo.TYPE_WIRED_HEADPHONES -> MR.images.ic_headphones

    AudioDeviceInfo.TYPE_USB_HEADSET, AudioDeviceInfo.TYPE_USB_DEVICE -> MR.images.ic_usb
    else -> MR.images.ic_brand_awareness_filled
  }

private fun AudioDeviceInfo.details(): String = "$productName id:$id name:${name?.localized()} type:$type sink:$isSink source:$isSource"

val AudioDeviceInfo.name: StringResource?
  get() = when (this.type) {
    AudioDeviceInfo.TYPE_BUILTIN_EARPIECE -> MR.strings.audio_device_earpiece
    AudioDeviceInfo.TYPE_BUILTIN_SPEAKER -> MR.strings.audio_device_speaker

    AudioDeviceInfo.TYPE_BLUETOOTH_SCO,
    AudioDeviceInfo.TYPE_BLUETOOTH_A2DP,
    AudioDeviceInfo.TYPE_BLE_HEADSET,
    AudioDeviceInfo.TYPE_BLE_SPEAKER -> if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
      // Use product name instead
      null
    } else {
      MR.strings.audio_device_bluetooth
    }

    AudioDeviceInfo.TYPE_WIRED_HEADPHONES -> MR.strings.audio_device_wired_headphones

    AudioDeviceInfo.TYPE_USB_HEADSET, AudioDeviceInfo.TYPE_USB_DEVICE -> null // Use product name instead
    else -> null // Use product name instead
  }
