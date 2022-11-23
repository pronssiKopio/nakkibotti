package s1.telegrambots.nakki

import scala.collection.mutable.Buffer

object taskState extends Enumeration {
  type taskState = Value
  val notAvailable, available, waitingForMembers, workInProgress, complete = Value
}
import taskState._

// make sure this has been changed for the new version
class Task(var name: String, var maxPpl: Int, val event: Event, val id: Int){
  var description: String = ""
  var points: Int = 0
  var status : taskState = notAvailable
  var users = Buffer[Participant]()

  override def toString = s"$id: $name, $points points, ${users.size}/$maxPpl people, ${status2emoji(status)}"
  val status2emoji = Map(
    notAvailable -> "🔜",
    available -> "🟥",
    waitingForMembers -> "🔻",
    workInProgress -> "🟨",
    complete -> "🟩"
  )

  def addUser(participant: Participant): Unit = {
    if (users.size < maxPpl && status != notAvailable) {
      users += participant
      if (users.size == maxPpl) status = workInProgress
      else status = waitingForMembers
    }
  }
}

