package s1.telegrambots.nakki

class User(telegramId: String, name: String, event: Event, var isAdmin: Boolean = false){
  var status = "free"
  var points = 0
}
