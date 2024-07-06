package chat.simplex.common.views.helpers

import InfoRow
import SectionBottomSpacer
import SectionItemViewSpaceBetween
import SectionTextFooter
import SectionView
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.material.Divider
import androidx.compose.material.Icon
import androidx.compose.material.LeadingIconTab
import androidx.compose.material.MaterialTheme
import androidx.compose.material.TabRow
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.AgentSMPServerStatsData
import chat.simplex.common.model.ChatController.chatModel
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.OnionHosts
import chat.simplex.common.model.PresentedServersSummary
import chat.simplex.common.model.RemoteHostInfo
import chat.simplex.common.model.SMPServerSubs
import chat.simplex.common.model.SMPServerSummary
import chat.simplex.common.model.SMPTotals
import chat.simplex.common.model.ServerAddress.Companion.parseServerAddress
import chat.simplex.common.model.ServerSessions
import chat.simplex.common.model.localTimestamp
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.datetime.Instant
import kotlin.math.floor
import kotlin.math.roundToInt
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

data class SubscriptionStatus(
  val color: Color,
  val variableValue: Float,
  val opacity: Float,
  val statusPercent: Float
)

fun subscriptionStatusColorAndPercentage(
  online: Boolean,
  onionHosts: OnionHosts,
  subs: SMPServerSubs,
  sess: ServerSessions
): SubscriptionStatus {

  fun roundedToQuarter(n: Float): Float {
    return when {
      n >= 1 -> 1f
      n <= 0 -> 0f
      else -> (n * 4).roundToInt() / 4f
    }
  }

  val activeColor: Color = if (onionHosts == OnionHosts.REQUIRED) Color(0xFF4B0082) else Color(0xFF00796B)
  val noConnColorAndPercent = SubscriptionStatus(Color(0xFFB0BEC5), 1f, 1f, 0f)
  val activeSubsRounded = roundedToQuarter(subs.shareOfActive)

  return if (online && subs.total > 0) {
    if (subs.ssActive == 0) {
      if (sess.ssConnected == 0) noConnColorAndPercent else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    } else { // ssActive > 0
      if (sess.ssConnected == 0) SubscriptionStatus(Color(0xFFFFA500), activeSubsRounded, subs.shareOfActive, subs.shareOfActive) // This would mean implementation error
      else SubscriptionStatus(activeColor, activeSubsRounded, subs.shareOfActive, subs.shareOfActive)
    }
  } else noConnColorAndPercent
}

@Composable
fun SubscriptionStatusIndicatorView(subs: SMPServerSubs, sess: ServerSessions, leadingPercentage: Boolean = false) {
  val netCfg = remember { chatModel.controller.getNetCfg() }
  val onionHosts = remember { netCfg.onionHosts }
  val statusColorAndPercentage = subscriptionStatusColorAndPercentage(chatModel.networkInfo.value.online, onionHosts, subs, sess)
  val pref = remember { chatModel.controller.appPrefs.networkShowSubscriptionPercentage }
  val percentageText = "${(floor(statusColorAndPercentage.statusPercent * 100)).toInt()}%"

  Row(verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(4.dp)) {
    if (pref.state.value && leadingPercentage) Text(percentageText, color = MaterialTheme.colors.secondary)
    SubscriptionStatusIcon(color = MaterialTheme.colors.primary, variableValue = statusColorAndPercentage.variableValue)
    if (pref.state.value && !leadingPercentage) Text(percentageText, color = MaterialTheme.colors.secondary)
  }
}

@Composable
fun SubscriptionStatusIndicator(click: (() -> Unit)? = null, ) {
  var subs by remember { mutableStateOf(SMPServerSubs.newSMPServerSubs) }
  var sess by remember { mutableStateOf(ServerSessions.newServerSessions) }
  var timerCounter by remember { mutableStateOf(0) }
  var timer: Job? by remember { mutableStateOf(null) }

  val initialInterval: Duration = 1.seconds
  val regularInterval: Duration = 3.seconds
  val initialPhaseDuration: Duration = 10.seconds

  val scope = rememberCoroutineScope()

  fun setServersSummary() {
    withBGApi {
      val summary: PresentedServersSummary? = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())

      if (summary != null) {
        subs = summary.allUsersSMP.smpTotals.subs
        sess = summary.allUsersSMP.smpTotals.sessions
      }
    }
  }

  fun stopTimer() {
    timer?.cancel()
    timer = null
  }

  fun switchToRegularTimer() {
    stopTimer()
    timer = timer ?: scope.launch {
      while (true) {
        delay(regularInterval.inWholeMilliseconds)
        setServersSummary()
      }
    }
  }

  fun startInitialTimer() {
    timer = timer ?: scope.launch {
      while (true) {
        delay(initialInterval.inWholeMilliseconds)
        setServersSummary()
        timerCounter++
        if (timerCounter *  initialInterval.inWholeSeconds >= initialPhaseDuration.inWholeSeconds) {
          switchToRegularTimer()
        }
      }
    }
  }

  DisposableEffect(Unit) {
    onDispose {
      stopTimer()
      scope.cancel()
    }
  }

  LaunchedEffect(Unit) {
    startInitialTimer()
  }

  Row(if (click != null) Modifier.clickable(onClick = click) else Modifier) {
    SubscriptionStatusIndicatorView(subs = subs, sess = sess)
  }
}

enum class PresentedUserCategory {
  CURRENT_USER, ALL_USERS
}

enum class PresentedServerType {
  SMP, XFTP
}

fun numOrDash(n: Int): String {
  return if (n == 0) "-" else n.toString()
}

@Composable
private fun ServerSessionsView(sess: ServerSessions) {
  SectionView(generalGetString(MR.strings.servers_info_transport_sessions_section_header)) {
    InfoRow(
      generalGetString(MR.strings.number_of_servers_connected),
      numOrDash(sess.ssConnected)
    )
    InfoRow(
      generalGetString(MR.strings.number_of_servers_with_connection_errors),
      numOrDash(sess.ssErrors)
    )
    InfoRow(
      generalGetString(MR.strings.number_of_servers_connecting),
      numOrDash(sess.ssConnecting)
    )
  }
}

private fun serverAddress(server: String): String {
  val address =  parseServerAddress(server)

  return address?.hostnames?.first() ?: server
}

@Composable
private fun SmpServerView(srvSumm: SMPServerSummary, statsStartedAt: Instant) {
  SectionItemViewSpaceBetween(
    click = {
      ModalManager.start.showCustomModal { _ -> Text("Server") }
    }
  ) {
    Column(
      modifier = Modifier.fillMaxWidth(),
      verticalArrangement = Arrangement.Center,
    ) {
      Row(
        modifier = Modifier.fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween
      ) {
        Text(serverAddress(srvSumm.smpServer))
        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
          if (srvSumm.subs != null && srvSumm.sessions != null) {
            SubscriptionStatusIndicatorView(subs = srvSumm.subs, sess = srvSumm.sessions, leadingPercentage = true)
          }
          RowLinkIcon("see server details")
        }
      }
    }
  }
}

@Composable
private fun SmpServersListView(servers: List<SMPServerSummary>, statsStartedAt: Instant, header: String? = null, footer: String? = null) {
  val sortedServers = servers.sortedWith(compareBy<SMPServerSummary> { !it.hasSubs }
    .thenBy { serverAddress(it.smpServer) })

  SectionView(header) {
    sortedServers.map { svr -> SmpServerView(svr, statsStartedAt)}
  }
  if (footer != null) {
    SectionTextFooter(
      footer
    )
  }
}

@Composable
private fun RowLinkIcon(contentDescription: String) {
  return Icon(painterResource(MR.images.ic_chevron_right), contentDescription, tint = MaterialTheme.colors.secondary)
}

@Composable
private fun SMPStatsView(stats: AgentSMPServerStatsData, statsStartedAt: Instant) {
  SectionView(generalGetString(MR.strings.servers_info_statistics_section_header)) {
    InfoRow(
      generalGetString(MR.strings.servers_info_messages_sent),
      numOrDash(stats._sentDirect + stats._sentViaProxy)
    )
    InfoRow(
      generalGetString(MR.strings.servers_info_messages_received),
      numOrDash(stats._recvMsgs)
    )
    SectionItemViewSpaceBetween(
      click = {
        ModalManager.start.showCustomModal { _ -> Text("Hello") }
      }
    ) {
      Row(
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.SpaceBetween,
        modifier = Modifier.fillMaxWidth(),
        ) {
        Text(text = generalGetString(MR.strings.servers_info_details), color = MaterialTheme.colors.onBackground)
        RowLinkIcon("see details")
      }
    }
  }
  SectionTextFooter(
    String.format(stringResource(MR.strings.servers_info_private_data_disclaimer), localTimestamp(statsStartedAt))
  )
}

@Composable
private fun SMPSubscriptionsSection(totals: SMPTotals) {
  Column {
    Row(Modifier.padding(start = DEFAULT_PADDING, bottom = 5.dp), verticalAlignment = Alignment.CenterVertically, horizontalArrangement = Arrangement.spacedBy(6.dp)) {
      Text(generalGetString(MR.strings.servers_info_subscriptions_section_header), color = MaterialTheme.colors.secondary, style = MaterialTheme.typography.body2, fontSize = 12.sp)
      SubscriptionStatusIndicatorView(totals.subs, totals.sessions)
    }
    Column(Modifier.padding(PaddingValues()).fillMaxWidth()) {
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_connections_subscribed),
        numOrDash(totals.subs.ssActive)
      )
      InfoRow(
        generalGetString(MR.strings.servers_info_subscriptions_total),
        numOrDash(totals.subs.total)
      )
    }
  }
}

@Composable
fun ModalData.ServersSummaryView(rh: RemoteHostInfo?) {
  Column(
    Modifier.fillMaxSize(),
  ) {
    var timer: Job? by remember { mutableStateOf(null) }
    var serversSummary by remember { mutableStateOf<PresentedServersSummary?>(null) }
    //val selectedUserCategory = remember { stateGetOrPut("selection") { PresentedUserCategory.ALL_USERS } }
    val selectedServerType = remember { stateGetOrPut("serverTypeSelection") { PresentedServerType.SMP } }

    val scope = rememberCoroutineScope()
    val fetchInterval: Duration = 1.seconds

    fun getServersSummary() {
      withBGApi {
        serversSummary = chatModel.controller.getAgentServersSummary(chatModel.remoteHostId())
      }
    }

    LaunchedEffect(Unit) {
      if (chatModel.users.count { u -> u.user.activeUser || !u.user.hidden } == 1
      ) {
        //selectedUserCategory.value = PresentedUserCategory.CURRENT_USER
      }
      getServersSummary()
      timer = timer ?: scope.launch {
        while (true) {
          delay(fetchInterval.inWholeMilliseconds)
          getServersSummary()
        }
      }
    }

    DisposableEffect(Unit) {
      onDispose {
        timer?.cancel()
        timer = null
        scope.cancel()
      }
    }

    Column(
      Modifier.fillMaxSize(),
    ) {
      Box(contentAlignment = Alignment.Center) {
        val bottomPadding = DEFAULT_PADDING
        AppBarTitle(
          stringResource(MR.strings.servers_info),
          hostDevice(rh?.remoteHostId),
          bottomPadding = bottomPadding
        )
      }
      if (serversSummary == null) {
        return Text("No info, try to reload")
      }

//      val userTabTitles = PresentedUserCategory.entries.map {
//        when (it) {
//          PresentedUserCategory.CURRENT_USER ->
//            stringResource(MR.strings.current_user)
//
//          PresentedUserCategory.ALL_USERS ->
//            stringResource(MR.strings.all_users)
//        }
//      }

      val serverTypeTabTitles = PresentedServerType.entries.map {
        when (it) {
          PresentedServerType.SMP ->
            stringResource(MR.strings.messages_section_title)

          PresentedServerType.XFTP ->
            stringResource(MR.strings.files_and_media_section)
        }
      }

      val serverTypePagerState = rememberPagerState(
        initialPage = selectedServerType.value.ordinal,
        initialPageOffsetFraction = 0f
      ) { PresentedServerType.entries.size }

      KeyChangeEffect(serverTypePagerState.currentPage) {
        selectedServerType.value = PresentedServerType.values()[serverTypePagerState.currentPage]
      }
      TabRow(
        selectedTabIndex = serverTypePagerState.currentPage,
        backgroundColor = Color.Transparent,
        contentColor = MaterialTheme.colors.primary,
      ) {
        serverTypeTabTitles.forEachIndexed { index, it ->
          LeadingIconTab(
            selected = serverTypePagerState.currentPage == index,
            onClick = {
              scope.launch {
                serverTypePagerState.animateScrollToPage(index)
              }
            },
            text = { Text(it, fontSize = 13.sp) },
            icon = {
              Icon(
                if (PresentedServerType.SMP.ordinal == index) painterResource(MR.images.ic_mail) else painterResource(MR.images.ic_download),
                it
              )
            },
            selectedContentColor = MaterialTheme.colors.primary,
            unselectedContentColor = MaterialTheme.colors.secondary,
          )
        }
      }

      HorizontalPager(state = serverTypePagerState, Modifier.fillMaxSize(), verticalAlignment = Alignment.Top) { index ->
        ColumnWithScrollBar(
          Modifier
            .fillMaxSize(),
          verticalArrangement = Arrangement.Top) {
          Spacer(Modifier.height(DEFAULT_PADDING))
          when (index) {
            PresentedServerType.SMP.ordinal -> {
              val castedSummary = serversSummary!!
              val smpSummary = castedSummary.currentUserSMP;
              val totals = smpSummary.smpTotals
              val currentlyUsedSMPServers = smpSummary.currentlyUsedSMPServers
              val previouslyUsedSMPServers = smpSummary.previouslyUsedSMPServers
              val proxySMPServers = smpSummary.onlyProxiedSMPServers
              val statsStartedAt = castedSummary.statsStartedAt

              SMPStatsView(totals.stats, statsStartedAt)
              Divider(
                Modifier.padding(
                  start = DEFAULT_PADDING_HALF,
                  top = 32.dp,
                  end = DEFAULT_PADDING_HALF,
                  bottom = 30.dp
                )
              )
              SMPSubscriptionsSection(totals)
              Divider(
                Modifier.padding(
                  start = DEFAULT_PADDING_HALF,
                  top = 32.dp,
                  end = DEFAULT_PADDING_HALF,
                  bottom = 30.dp
                )
              )

              if (currentlyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(currentlyUsedSMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_connected_servers_section_header))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              if (previouslyUsedSMPServers.isNotEmpty()) {
                SmpServersListView(previouslyUsedSMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_previously_connected_servers_section_header))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              if (proxySMPServers.isNotEmpty()) {
                SmpServersListView(proxySMPServers, statsStartedAt, generalGetString(MR.strings.servers_info_proxied_servers_section_header), generalGetString(MR.strings.servers_info_proxied_servers_section_footer))
                Divider(
                  Modifier.padding(
                    start = DEFAULT_PADDING_HALF,
                    top = 32.dp,
                    end = DEFAULT_PADDING_HALF,
                    bottom = 30.dp
                  )
                )
              }

              ServerSessionsView(totals.sessions)
            }

            PresentedServerType.XFTP.ordinal -> {
              Text("XFTP")
            }

          }
          Divider(
            Modifier.padding(
              start = DEFAULT_PADDING_HALF,
              top = 32.dp,
              end = DEFAULT_PADDING_HALF,
              bottom = 30.dp
            )
          )

          SectionItemViewSpaceBetween {
            Row {
              Text(
                stringResource(MR.strings.servers_info_reconnect_all_servers_button),
                modifier = Modifier.clickable() {
                  AlertManager.shared.showAlertDialog(
                    title = generalGetString(MR.strings.servers_info_reconnect_servers_title),
                    text = generalGetString(MR.strings.servers_info_reconnect_servers_message),
                    dismissText = generalGetString(MR.strings.servers_info_modal_dismiss),
                    destructive = true,
                    onConfirm = {
                      withBGApi {
                        val success = controller.reconnectAllServers(rh?.remoteHostId)

                        if (!success) {
                          AlertManager.shared.showAlertMsg(
                            title = generalGetString(MR.strings.servers_info_modal_error_title),
                            text = generalGetString(MR.strings.servers_info_reconnect_servers_error)
                          )
                        }
                      }
                    }
                  )
                },
                color = MaterialTheme.colors.primary
              )
            }
          }

          SectionItemViewSpaceBetween {
            Row {
              Text(
                stringResource(MR.strings.servers_info_reset_stats),
                modifier = Modifier.clickable() {
                  AlertManager.shared.showAlertDialog(
                    title = generalGetString(MR.strings.servers_info_reset_stats_alert_title),
                    text = generalGetString(MR.strings.servers_info_reset_stats_alert_message),
                    dismissText = generalGetString(MR.strings.servers_info_modal_dismiss),
                    destructive = true,
                    onConfirm = {
                      withBGApi {
                        val success = controller.resetAgentServersStats(rh?.remoteHostId)

                        if (!success) {
                          AlertManager.shared.showAlertMsg(
                            title = generalGetString(MR.strings.servers_info_modal_error_title),
                            text = generalGetString(MR.strings.servers_info_reset_stats_alert_error_title)
                          )
                        }
                      }
                    }
                  )
                },
                color = MaterialTheme.colors.primary
              )
            }
          }

          SectionBottomSpacer()
        }
      }
    }

    }
}