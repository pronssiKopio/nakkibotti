package s1.telegrambots.nakki

import scala.collection.mutable.Buffer

object taskStatus extends Enumeration {
  type taskStatus = Value
  val notAvailable, available, waitingForMembers, workInProgress, complete = Value
}
import taskStatus._

// make sure this has been changed for the new version
class Task(var name: String, var maxPpl: Int, val event: Event, val id: Int){
  var description: String = ""
  var points: Int = 0
  var status : taskStatus = available
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
    if (users.size < maxPpl && status != notAvailable && participant.status == participantStatus.free) {
      users += participant
      if (users.size == maxPpl) status = workInProgress
      else status = waitingForMembers
      participant.status = participantStatus.busy
      true
    }
    else false
  }

  def finish(): Unit = {
    this.status = complete
    this.users.foreach(p => {
      p.status = participantStatus.free
      p.points += this.points
    })
    this.users.empty
  }


}

