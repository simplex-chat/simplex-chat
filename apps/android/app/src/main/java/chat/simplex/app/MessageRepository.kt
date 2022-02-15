package chat.simplex.app

import android.util.Log

typealias Controller = Long
typealias Store = Long
external fun chatInit(filesDir: String): Store
external fun chatGetUser(controller: Store) : String
external fun chatCreateUser(controller: Store, data: String) : String
external fun chatStart(controller: Store) : Controller
external fun chatSendCmd(controller: Controller, msg: String) : String


class MessageRepository(filesDir: String) {

//    private val store = chatInit(filesDir)
//    private val controller: Controller by lazy { // Maybe this shouldn't be lazy
//        this.maybeCreateUser()
//        chatStart(store)
//    }

//    fun maybeCreateUser() {
//        // create user if needed
//        if(chatGetUser(store) == "{}") {
//            chatCreateUser(store, """
//                    {"displayName": "test", "fullName": "android test"}
//                    """.trimIndent())
//        }
//        Log.d("SIMPLEX (user)", chatGetUser(store))
//    }

    fun sendCmd(cmd: String) {
        println(cmd)
//        Log.d("SIMPLEXMPLEX SEND", chatSendCmd(controller, cmd))
    }
}
