package chat.simplex.app

import chat.simplex.common.views.call.*
import dev.onvoid.webrtc.media.audio.AudioDevice
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import org.junit.Assume.assumeTrue
import java.util.concurrent.TimeUnit
import kotlin.test.*

class AudioDeviceChoiceTest {
  private class TestDevice(name: String, descriptor: String?) : AudioDevice(name, descriptor)

  // Follows CubebAudioDeviceModule: the selected device is used while listed, otherwise the default if fallback is enabled
  private class Module(var fallback: Boolean = true) {
    val default = TestDevice("default (Built-in)", null)
    val builtIn = TestDevice("Built-in", "builtin")
    val headset = TestDevice("Headset", "usb-1")
    var listed = mutableListOf(builtIn, headset)
    var staleListed: List<AudioDevice>? = null
    var selectedId: String? = null
    val selections = ArrayList<String?>()

    fun devices(): List<AudioDevice> = listOf(default) + (staleListed ?: listed)
    fun inUse(): AudioDevice? = listed.firstOrNull { it.descriptor == selectedId } ?: if (selectedId == null || fallback) default else null
    fun select(device: AudioDevice) {
      selectedId = device.descriptor
      selections.add(device.descriptor)
    }
    fun selectDefault() {
      selectedId = null
      selections.add(null)
    }
    fun choice(saved: SavedAudioDevice?) = AudioDeviceChoice(AudioDeviceKind.Mic, saved, ::devices, ::inUse, ::select, ::selectDefault)
  }

  private val savedHeadset = SavedAudioDevice("usb-1", "Headset")

  @Test
  fun testListsDevicesWithoutSystemDefaultEntry() {
    val m = Module()
    assertEquals(listOf<AudioDevice>(m.builtIn, m.headset), m.choice(null).devices())
  }

  @Test
  fun testSelectsSavedDeviceByIdWithoutNotice() {
    val m = Module()
    val choice = m.choice(savedHeadset)
    assertNull(choice.update())
    assertEquals(m.headset, m.inUse())
    assertNull(choice.update())
    assertEquals(listOf<String?>("usb-1"), m.selections, "a selected device is not selected again")
  }

  @Test
  fun testSelectsSavedDeviceByNameWhenIdChanged() {
    val m = Module()
    val renumbered = TestDevice("Headset", "usb-2")
    m.listed = mutableListOf(m.builtIn, renumbered)
    assertNull(m.choice(savedHeadset).update())
    assertEquals(renumbered, m.inUse())
  }

  @Test
  fun testAmbiguousNameUsesSystemDefault() {
    val m = Module()
    val twin1 = TestDevice("Headset", "usb-2")
    val twin2 = TestDevice("Headset", "usb-3")
    m.listed = mutableListOf(m.builtIn, twin1, twin2)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Fallback), m.choice(savedHeadset).update())
    assertEquals(m.default, m.inUse())
    assertEquals(listOf<String?>("usb-1"), m.selections, "the saved ID is selected instead of a device sharing its name with another one")
  }

  @Test
  fun testIdMatchTakesPrecedenceOverName() {
    val m = Module()
    val sameName = TestDevice("Headset", "usb-9")
    m.listed = mutableListOf(sameName, m.headset)
    m.choice(savedHeadset).update()
    assertEquals(m.headset, m.inUse())
  }

  @Test
  fun testSavedDeviceAbsentAtStartIsSelectedAndUsedWhenListed() {
    val m = Module()
    m.listed.remove(m.headset)
    val choice = m.choice(savedHeadset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Fallback), choice.update())
    assertEquals(m.default, m.inUse())
    assertEquals(listOf<String?>("usb-1"), m.selections, "an absent device is selected by its saved ID")
    m.listed.add(m.headset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Connected), choice.update())
    assertEquals(m.headset, m.inUse())
    assertEquals(listOf<String?>("usb-1"), m.selections, "a device listed again is not selected again")
  }

  @Test
  fun testSavedDeviceAbsentAtStartWithoutFallbackUsesNoDevice() {
    val m = Module(fallback = false)
    m.listed.remove(m.headset)
    val choice = m.choice(savedHeadset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Unavailable), choice.update())
    assertNull(m.inUse())
    m.listed.add(m.headset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Connected), choice.update())
    assertEquals(m.headset, m.inUse())
  }

  @Test
  fun testUnplugAndReplugWithFallback() {
    val m = Module()
    val choice = m.choice(savedHeadset)
    assertNull(choice.update())
    m.listed.remove(m.headset)
    assertEquals(AudioDeviceState.Fallback, choice.update()?.state)
    assertNull(choice.update(), "an unchanged state is reported once")
    m.listed.add(m.headset)
    assertEquals(AudioDeviceState.Connected, choice.update()?.state)
    assertEquals(m.headset, m.inUse())
  }

  @Test
  fun testUnplugAndReplugWithoutFallback() {
    val m = Module(fallback = false)
    val choice = m.choice(savedHeadset)
    assertNull(choice.update())
    m.listed.remove(m.headset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Unavailable), choice.update())
    assertNull(m.inUse())
    m.listed.add(m.headset)
    assertEquals(AudioDeviceState.Connected, choice.update()?.state)
  }

  @Test
  fun testSavedDeviceInUseBeforeListedIsConnected() {
    val m = Module(fallback = false)
    val choice = m.choice(savedHeadset)
    assertNull(choice.update())
    m.listed.remove(m.headset)
    assertEquals(AudioDeviceState.Unavailable, choice.update()?.state)
    m.staleListed = m.listed.toList()
    m.listed.add(m.headset)
    assertEquals(AudioDeviceState.Connected, choice.update()?.state, "the device in use is reported before the device list includes it")
    m.staleListed = null
    assertNull(choice.update())
  }

  @Test
  fun testChoosingSystemDefaultForgetsSavedDevice() {
    val m = Module()
    val choice = m.choice(savedHeadset)
    m.listed.remove(m.headset)
    assertEquals(AudioDeviceState.Fallback, choice.update()?.state)
    choice.choose(null)
    m.listed.add(m.headset)
    assertNull(choice.update())
    assertEquals(m.default, m.inUse())
    assertEquals(listOf<String?>("usb-1", null), m.selections)
  }

  @Test
  fun testChoosingDeviceReportsItsLaterDisconnection() {
    val m = Module()
    val choice = m.choice(null)
    choice.choose(m.headset)
    assertEquals(m.headset, m.inUse())
    assertNull(choice.update())
    m.listed.remove(m.headset)
    assertEquals(AudioDeviceNotice(AudioDeviceKind.Mic, "Headset", AudioDeviceState.Fallback), choice.update())
  }

  @Test
  fun testSavesDeviceIdAndName() {
    assertEquals(savedHeadset, savedAudioDevice(Module().headset))
  }

  @Test
  fun testEngineFollowsSavedSpeakerAcrossUnplugWithFallback() = runBlocking {
    assertEquals(listOf(AudioDeviceState.Fallback, AudioDeviceState.Connected), savedSinkNoticesAcrossUnplug(fallback = true))
  }

  @Test
  fun testEngineReportsSavedSpeakerUnavailableWithoutFallback() = runBlocking {
    assertEquals(listOf(AudioDeviceState.Unavailable, AudioDeviceState.Connected), savedSinkNoticesAcrossUnplug(fallback = false))
  }

  @Test
  fun testEngineUsesNoSpeakerWhileSavedSpeakerAbsentAtStartWithoutFallback() = runBlocking {
    assumeTrue("Needs a PulseAudio server and pactl", runCatching { pactl("info") }.isSuccess)
    val sinkName = "simplex_test_${System.nanoTime()}"
    val notices = Channel<AudioDeviceNotice>(Channel.UNLIMITED)
    val engine = NativeCallEngine(CallAudioSettings(null, SavedAudioDevice(sinkName, sinkName), fallback = false), { notices.trySend(it) }, {})
    var sinkModule: String? = null
    try {
      assertEquals(AudioDeviceNotice(AudioDeviceKind.Speaker, sinkName, AudioDeviceState.Unavailable), nextNotice(notices))
      sinkModule = loadNullSink(sinkName)
      assertEquals(AudioDeviceNotice(AudioDeviceKind.Speaker, sinkName, AudioDeviceState.Connected), nextNotice(notices))
    } finally {
      engine.close().join()
      sinkModule?.let { pactl("unload-module", it) }
    }
  }

  // Needs a PulseAudio server and pactl, as CubebAudioDeviceModuleTest in webrtc-java.
  // The sink gets a unique name, as all null sinks share the name "Null Output" and would match by name.
  private suspend fun savedSinkNoticesAcrossUnplug(fallback: Boolean): List<AudioDeviceState> {
    assumeTrue("Needs a PulseAudio server and pactl", runCatching { pactl("info") }.isSuccess)
    val sinkName = "simplex_test_${System.nanoTime()}"
    val pluggedModule = loadNullSink(sinkName)
    var sinkModule: String? = pluggedModule
    val notices = Channel<AudioDeviceNotice>(Channel.UNLIMITED)
    var engine: NativeCallEngine? = null
    suspend fun nextNotice() = nextNotice(notices).also { assertEquals(AudioDeviceKind.Speaker, it.kind); assertEquals(sinkName, it.deviceName) }
    try {
      val lister = NativeCallEngine(CallAudioSettings(null, null, fallback), {}, {})
      val sink = try {
        lister.audioDevices(AudioDeviceKind.Speaker).first { it.descriptor == sinkName }
      } finally {
        lister.close().join()
      }
      engine = NativeCallEngine(CallAudioSettings(null, savedAudioDevice(sink), fallback), { notices.trySend(it) }, {})
      pactl("unload-module", pluggedModule)
      sinkModule = null
      val unplugged = nextNotice()
      sinkModule = loadNullSink(sinkName)
      return listOf(unplugged.state, nextNotice().state)
    } finally {
      engine?.close()?.join()
      sinkModule?.let { pactl("unload-module", it) }
    }
  }

  private suspend fun nextNotice(notices: Channel<AudioDeviceNotice>): AudioDeviceNotice = withTimeout(NOTICE_TIMEOUT_MS) { notices.receive() }

  private fun loadNullSink(name: String): String = pactl("load-module", "module-null-sink", "sink_name=$name", "sink_properties=device.description=$name")

  private fun pactl(vararg args: String): String {
    val process = ProcessBuilder(listOf("pactl") + args).redirectErrorStream(true).start()
    val output = process.inputStream.bufferedReader().readText().trim()
    check(process.waitFor(PACTL_TIMEOUT_S, TimeUnit.SECONDS) && process.exitValue() == 0) { "pactl ${args.joinToString(" ")}: $output" }
    return output
  }

  companion object {
    private const val NOTICE_TIMEOUT_MS = 10_000L
    private const val PACTL_TIMEOUT_S = 10L
  }
}
