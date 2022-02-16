package chat.simplex.app

sealed class Pages(val route: String) {
  object Home : Pages("home")
  object Terminal : Pages("terminal")
  object Welcome : Pages("welcome")
}