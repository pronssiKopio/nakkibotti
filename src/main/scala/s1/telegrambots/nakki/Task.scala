package s1.telegrambots.nakki

import scala.collection.mutable.Buffer

object taskState extends Enumeration {
  type taskState = Value
  val notAvailable, available, waitingForMembers, workInProgress, complete = Value
}
import taskState._

class Task(var name: String, var maxPpl: Int, event: Event){
  var description: String = ""
  var points: Int = 0
  var status : taskState = notAvailable
  val users = Buffer[Participant]()

  override def toString = name + ", "+ points + " points, " + status2emoji(status)
  val status2emoji = Map(notAvailable -> "🔜")
}

