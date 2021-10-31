package chat.simplex.app

class Protocol {

    companion object {
        // Used to load the 'app' library on application startup.
        init {
            System.loadLibrary("app")
        }

        /**
         * A native method that is implemented by the 'app' native library,
         * which is packaged with this application.
         */
        @JvmStatic
        external fun executeCommand(command: String): String
    }
}