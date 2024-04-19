package chat.simplex.common.views.call

import android.content.Context
import android.media.*
import android.media.AudioManager.OnCommunicationDeviceChangedListener
import android.os.Build
import androidx.annotation.RequiresApi
import androidx.compose.runtime.*
import chat.simplex.common.platform.*
import dev.icerock.moko.resources.ImageResource
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import java.util.concurrent.Executors

interface CallAudioDeviceManagerInterface {
  val devices: State<List<AudioDeviceInfo>>
  val currentDevice: MutableState<AudioDeviceInfo?>
  fun start()
  fun stop()
  // AudioDeviceInfo.AudioDeviceType
  fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyNonEarpiece: Boolean)
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
  override val devices: MutableState<List<AudioDeviceInfo>> = mutableStateOf(emptyList())
  override val currentDevice: MutableState<AudioDeviceInfo?> = mutableStateOf(null)

  private val audioCallback = object: AudioDeviceCallback() {
    override fun onAudioDevicesAdded(addedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Added audio devices: ${addedDevices.map { it.type }}")
      super.onAudioDevicesAdded(addedDevices)
      val oldDevices = devices.value
      devices.value = am.availableCommunicationDevices
      Log.d(TAG, "Added audio devices2: ${devices.value.map { it.type }}")

      if (devices.value.size - oldDevices.size > 0) {
        selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.supportsVideo() == true, false)
      }
    }

    override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Removed audio devices: ${removedDevices.map { it.type }}")
      super.onAudioDevicesRemoved(removedDevices)
      devices.value = am.availableCommunicationDevices
    }
  }

  private val listener: OnCommunicationDeviceChangedListener = OnCommunicationDeviceChangedListener { device ->
    devices.value = am.availableCommunicationDevices
    currentDevice.value = device
  }

  override fun start() {
    am.registerAudioDeviceCallback(audioCallback, null)
    am.addOnCommunicationDeviceChangedListener(Executors.newSingleThreadExecutor(), listener)
  }

  override fun stop() {
    am.unregisterAudioDeviceCallback(audioCallback)
    am.removeOnCommunicationDeviceChangedListener(listener)
  }

  override fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyNonEarpiece: Boolean) {
    Log.d(TAG, "selectLastExternalDeviceOrDefault: set audio mode, speaker enabled: $speaker")
    am.mode = AudioManager.MODE_IN_COMMUNICATION
    val commDevice = am.communicationDevice
    if (keepAnyNonEarpiece && commDevice != null && commDevice.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE) {
      // some external device or speaker selected already, no need to change it
      return
    }

    val preferredSecondaryDevice = if (speaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    val externalDevice = devices.value.lastOrNull { it.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER && it.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE }
    // External device already selected
    if (externalDevice != null && externalDevice.type == am.communicationDevice?.type) {
      return
    }
    if (externalDevice != null) {
      am.setCommunicationDevice(externalDevice)
    } else if (am.communicationDevice?.type != preferredSecondaryDevice) {
      am.availableCommunicationDevices.firstOrNull { it.type == preferredSecondaryDevice }?.let {
        am.setCommunicationDevice(it)
      }
    }
  }

  override fun selectDevice(id: Int) {
    am.mode = AudioManager.MODE_IN_COMMUNICATION
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
      val wasSize = devices.value.size
      devices.value += addedDevices.filter { it.hasSupportedType() }
      val addedCount = devices.value.size - wasSize
      if (addedCount > 0 && chatModel.activeCall.value?.callState == CallState.Connected) {
        // Setting params in Connected state makes sure that Bluetooth will NOT be broken on Android < 12
        selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.supportsVideo() == true, false)
      }
    }

    override fun onAudioDevicesRemoved(removedDevices: Array<out AudioDeviceInfo>) {
      Log.d(TAG, "Removed audio devices: ${removedDevices.map { it.type }}")
      super.onAudioDevicesRemoved(removedDevices)
      val wasSize = devices.value.size
      devices.value = devices.value.filterNot { removedDevices.any { rm -> rm.id == it.id } }
      //val removedCount = wasSize - devices.value.size
      //if (devices.value.count { it.hasSupportedType() } == 0 && chatModel.activeCall.value?.callState == CallState.Connected) {
        // Setting params in Connected state makes sure that Bluetooth will NOT be broken on Android < 12
        //selectLastExternalDeviceOrDefault(chatModel.activeCall.value?.supportsVideo() == true, true)
      //}
    }
  }

  override fun start() {
    am.registerAudioDeviceCallback(audioCallback, null)
  }

  override fun stop() {
    am.unregisterAudioDeviceCallback(audioCallback)
  }

  override fun selectLastExternalDeviceOrDefault(speaker: Boolean, keepAnyNonEarpiece: Boolean) {
    Log.d(TAG, "selectLastExternalDeviceOrDefault: set audio mode, speaker enabled: $speaker")
    val preferredSecondaryDevice = if (speaker) AudioDeviceInfo.TYPE_BUILTIN_SPEAKER else AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    val externalDevice = devices.value.lastOrNull { it.hasSupportedType() && it.isSink }
    if (externalDevice != null) {
      am.isSpeakerphoneOn = false
      am.startBluetoothSco()
      adaptToCurrentlyActiveDevice(externalDevice)
    } else {
      am.stopBluetoothSco()
      am.isSpeakerphoneOn = preferredSecondaryDevice == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
      val newCurrentDevice = devices.value.firstOrNull { it.type == preferredSecondaryDevice }
      adaptToCurrentlyActiveDevice(newCurrentDevice)
    }
    am.isBluetoothScoOn = am.isBluetoothScoAvailableOffCall && externalDevice != null
  }

  override fun selectDevice(id: Int) {
    val device = devices.value.lastOrNull { it.id == id }
    val isExternalDevice = device != null && device.type != AudioDeviceInfo.TYPE_BUILTIN_SPEAKER && device.type != AudioDeviceInfo.TYPE_BUILTIN_EARPIECE
    if (isExternalDevice) {
      am.isSpeakerphoneOn = false
      am.startBluetoothSco()
      adaptToCurrentlyActiveDevice(device)
    } else {
      am.stopBluetoothSco()
      am.isSpeakerphoneOn = device?.type == AudioDeviceInfo.TYPE_BUILTIN_SPEAKER
      adaptToCurrentlyActiveDevice(device)
    }
    am.isBluetoothScoOn = am.isBluetoothScoAvailableOffCall && isExternalDevice
  }

  private fun adaptToCurrentlyActiveDevice(newCurrentDevice: AudioDeviceInfo?) {
    currentDevice.value = newCurrentDevice
  }

  private fun AudioDeviceInfo.hasSupportedType(): Boolean = when (type) {
    AudioDeviceInfo.TYPE_BLUETOOTH_SCO -> true
    AudioDeviceInfo.TYPE_BLE_HEADSET -> true
    AudioDeviceInfo.TYPE_BLE_SPEAKER -> true
    AudioDeviceInfo.TYPE_WIRED_HEADPHONES -> true
    else -> false
  }
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

val AudioDeviceInfo.name: StringResource?
  get() = when (this.type) {
    AudioDeviceInfo.TYPE_BUILTIN_EARPIECE -> MR.strings.audio_device_earpiece
    AudioDeviceInfo.TYPE_BUILTIN_SPEAKER -> MR.strings.audio_device_speaker

    AudioDeviceInfo.TYPE_BLUETOOTH_SCO,
    AudioDeviceInfo.TYPE_BLUETOOTH_A2DP,
    AudioDeviceInfo.TYPE_BLE_HEADSET,
    AudioDeviceInfo.TYPE_BLE_SPEAKER -> null // Use product name instead

    AudioDeviceInfo.TYPE_WIRED_HEADPHONES -> MR.strings.audio_device_wired_headphones

    AudioDeviceInfo.TYPE_USB_HEADSET, AudioDeviceInfo.TYPE_USB_DEVICE -> null // Use product name instead
    else -> null // Use product name instead
  }

