package s1.telegrambots.nakki

import scala.collection.mutable.Buffer

class Task(var name: String, var description: String, var points: Int, var maxPpl: Int, event: Event){
  var status = "not_available"
  val users = Buffer[User]()
}
