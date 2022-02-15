package chat.simplex.app

import android.app.Application
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.getValue
import androidx.compose.runtime.setValue
import androidx.lifecycle.AndroidViewModel
import androidx.lifecycle.LiveData


class SimplexViewModel(application: Application) : AndroidViewModel(application) {
    private val messageRepository = getApplication<SimplexApp>().messageRepository
    val terminalLog = messageRepository.terminalLog

    fun onCmdEntered(cmd: String){
        messageRepository.sendCmd(cmd)
    }
}
