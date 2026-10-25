package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.usersettings.networkAndServers.*
import chat.simplex.res.*
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class NetworkServersUnitTest {

  private val smpServer = UserServer(remoteHostId = null, serverId = 1, server = "smp://abcd@smp8.simplex.im", preset = true, tested = true, enabled = true, deleted = false)
  private val xftpServer = UserServer(remoteHostId = null, serverId = 4, server = "xftp://abcd@xftp8.simplex.im", preset = true, tested = true, enabled = true, deleted = false)
  private val operator1 = ServerOperator.sampleData1
  private val operator2 = ServerOperator.sampleData1.copy(
    operatorId = 2,
    operatorTag = OperatorTag.Flux,
    tradeName = "Flux",
    legalName = "Flux Operations",
    serverDomains = listOf("simplexonflux.com"),
    conditionsAcceptance = ConditionsAcceptance.Required(deadline = null),
    enabled = false
  )
  private val servers = listOf(
    UserOperatorServers(operator = operator1, smpServers = listOf(smpServer), xftpServers = listOf(xftpServer)),
    UserOperatorServers(operator = operator2, smpServers = listOf(), xftpServers = listOf()),
    UserOperatorServers(operator = null, smpServers = listOf(smpServer.copy(serverId = 5, server = "smp://efgh@smp.example.com", preset = false)), xftpServers = listOf())
  )
  private val user = UserRef(userId = 1, localDisplayName = "alice", activeUser = true, showNtfs = true)

  private fun withFirstSmpServerEnabled(enabled: Boolean): List<UserOperatorServers> =
    servers.mapIndexed { i, s -> if (i == 0) s.copy(smpServers = listOf(smpServer.copy(enabled = enabled))) else s }

  private fun forUser(text: String) = "${String.format(generalGetString(MR.strings.for_chat_profile), "alice")} $text"

  @Test
  fun unchangedServersCannotBeSaved() {
    assertFalse(serversCanBeSaved(servers, servers.toList(), listOf()))
  }

  @Test
  fun changedServersWithoutErrorsCanBeSaved() {
    assertTrue(serversCanBeSaved(servers, withFirstSmpServerEnabled(false), listOf()))
  }

  @Test
  fun changedServersWithErrorsCannotBeSaved() {
    assertFalse(serversCanBeSaved(servers, withFirstSmpServerEnabled(false), listOf(UserServersError.NoServers(ServerProtocol.SMP, null))))
  }

  @Test
  fun unchangedServersWithErrorsCannotBeSaved() {
    assertFalse(serversCanBeSaved(servers, servers, listOf(UserServersError.NoServers(ServerProtocol.SMP, null))))
  }

  @Test
  fun serverToggledBackCannotBeSaved() {
    val changed = withFirstSmpServerEnabled(false)
    val toggledBack = changed.mapIndexed { i, s -> if (i == 0) s.copy(smpServers = s.smpServers.map { it.copy(enabled = true) }) else s }
    assertFalse(serversCanBeSaved(servers, toggledBack, listOf()))
  }

  @Test
  fun noSmpServersError() {
    val text = generalGetString(MR.strings.no_message_servers_configured)
    assertEquals(text, globalSMPServersError(listOf(UserServersError.NoServers(ServerProtocol.SMP, null))))
    assertEquals(forUser(text), globalSMPServersError(listOf(UserServersError.NoServers(ServerProtocol.SMP, user))))
  }

  @Test
  fun missingSmpRoleErrors() {
    assertEquals(
      generalGetString(MR.strings.no_message_servers_configured_for_receiving),
      globalSMPServersError(listOf(UserServersError.StorageMissing(ServerProtocol.SMP, null)))
    )
    assertEquals(
      forUser(generalGetString(MR.strings.no_message_servers_configured_for_private_routing)),
      globalSMPServersError(listOf(UserServersError.ProxyMissing(ServerProtocol.SMP, user)))
    )
  }

  @Test
  fun missingXftpRoleErrors() {
    assertEquals(
      generalGetString(MR.strings.no_media_servers_configured_for_sending),
      globalXFTPServersError(listOf(UserServersError.StorageMissing(ServerProtocol.XFTP, null)))
    )
    assertEquals(
      forUser(generalGetString(MR.strings.no_media_servers_configured_for_private_routing)),
      globalXFTPServersError(listOf(UserServersError.ProxyMissing(ServerProtocol.XFTP, user)))
    )
  }

  @Test
  fun xftpErrorIsNotSmpError() {
    val errors = listOf(UserServersError.NoServers(ServerProtocol.XFTP, null))
    assertNull(globalSMPServersError(errors))
    assertEquals(generalGetString(MR.strings.no_media_servers_configured), globalXFTPServersError(errors))
  }

  @Test
  fun duplicateServerIsNotGlobalError() {
    val errors = listOf(UserServersError.DuplicateServer(ServerProtocol.SMP, "smp://abcd@smp8.simplex.im", "smp8.simplex.im"))
    assertNull(globalSMPServersError(errors))
    assertEquals(emptyList<String>(), globalServersErrors(errors))
  }

  @Test
  fun firstMatchingSmpErrorWins() {
    val errors = listOf(
      UserServersError.DuplicateServer(ServerProtocol.SMP, "smp://abcd@smp8.simplex.im", "smp8.simplex.im"),
      UserServersError.NoServers(ServerProtocol.XFTP, null),
      UserServersError.StorageMissing(ServerProtocol.SMP, null),
      UserServersError.NoServers(ServerProtocol.SMP, null)
    )
    assertEquals(generalGetString(MR.strings.no_message_servers_configured_for_receiving), globalSMPServersError(errors))
  }

  @Test
  fun noErrorsMeansNoGlobalError() {
    assertNull(globalSMPServersError(listOf()))
    assertNull(globalXFTPServersError(listOf()))
  }

  @Test
  fun globalErrorsListBothProtocolsInOrder() {
    val errors = listOf(
      UserServersError.NoServers(ServerProtocol.XFTP, null),
      UserServersError.DuplicateChatRelayAddress("relay", "relay.example.com"),
      UserServersError.NoServers(ServerProtocol.SMP, null)
    )
    assertEquals(
      listOf(generalGetString(MR.strings.no_media_servers_configured), generalGetString(MR.strings.no_message_servers_configured)),
      globalServersErrors(errors)
    )
  }

  @Test
  fun duplicateHosts() {
    assertEquals(emptySet<String>(), findDuplicateHosts(listOf()))
    assertEquals(emptySet<String>(), findDuplicateHosts(listOf(UserServersError.NoServers(ServerProtocol.SMP, null))))
    assertEquals(
      setOf("smp8.simplex.im"),
      findDuplicateHosts(listOf(UserServersError.DuplicateServer(ServerProtocol.SMP, "smp://abcd@smp8.simplex.im", "smp8.simplex.im")))
    )
    assertEquals(
      setOf("smp8.simplex.im", "xftp8.simplex.im"),
      findDuplicateHosts(
        listOf(
          UserServersError.DuplicateServer(ServerProtocol.SMP, "smp://abcd@smp8.simplex.im", "smp8.simplex.im"),
          UserServersError.NoServers(ServerProtocol.SMP, null),
          UserServersError.DuplicateServer(ServerProtocol.SMP, "smp://efgh@smp8.simplex.im", "smp8.simplex.im"),
          UserServersError.DuplicateServer(ServerProtocol.XFTP, "xftp://abcd@xftp8.simplex.im", "xftp8.simplex.im"),
          UserServersError.DuplicateChatRelayAddress("relay", "relay.example.com")
        )
      )
    )
  }

  @Test
  fun conditionsAcceptanceIsUpdatedForMatchingOperatorOnly() {
    val usvs = mutableStateOf(servers)
    val accepted = ConditionsAcceptance.Accepted(acceptedAt = null, autoAccepted = true)
    val updatedOperator2 = operator2.copy(conditionsAcceptance = accepted, tradeName = "Renamed", enabled = true)
    val unknownOperator = operator1.copy(operatorId = 99, conditionsAcceptance = ConditionsAcceptance.Required(deadline = null))
    updateOperatorsConditionsAcceptance(usvs, listOf(unknownOperator, updatedOperator2))
    assertEquals(
      listOf(servers[0], servers[1].copy(operator = operator2.copy(conditionsAcceptance = accepted)), servers[2]),
      usvs.value
    )
  }

  @Test
  fun conditionsAcceptanceUpdateWithoutMatchesKeepsServers() {
    val usvs = mutableStateOf(servers)
    updateOperatorsConditionsAcceptance(usvs, listOf())
    assertEquals(servers, usvs.value)
  }
}
