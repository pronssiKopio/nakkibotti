package s1.telegrambots.nakki

import scala.collection.mutable.Buffer

class Task(var name: String, var maxPpl: Int, event: Event){
  var description: String = ""
  var points: Int = 0
  var status = "not_available"
  val users = Buffer[User]()

  override def toString = name + ", "+ points + " points, " + status2emoji(status)
  val status2emoji = Map("not_available" -> "🔜")
}
