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
  var status : taskState = available
  var users = Buffer[Participant]()

  override def toString = s"$id: $name, ${users.size}/$maxPpl people, $points points, ${status2emoji(status)}"
  val status2emoji = Map(
    notAvailable -> "🔜",
    available -> "🟥",
    waitingForMembers -> "🔻",
    workInProgress -> "🟨",
    complete -> "🟩"
  )

  def addUser(participant: Participant): Boolean = {
    if (users.size < maxPpl && status != notAvailable && participant.state == participantState.free) {
      users += participant
      if (users.size == maxPpl) status = workInProgress
      else status = waitingForMembers
      participant.state = participantState.busy
      true
    }
    else false
  }

  def finish(): Unit = {
    this.status = complete
    this.users.foreach(p => {
      p.state = participantState.free
      p.points += this.points
    })
    this.users.empty
  }


}

