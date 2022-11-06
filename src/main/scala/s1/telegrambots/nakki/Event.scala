package s1.telegrambots.nakki

import util.Random.alphanumeric

class Event(var name: String, var description: String, id: Int ) {

  val accessCode = alphanumeric.take(Constants.AccessCodeLength).mkString
}
