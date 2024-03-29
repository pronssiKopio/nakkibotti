package s1.telegrambots.nakki

import s1.telegrambots.nakki.taskStatus._

import util.Random.alphanumeric
import collection.mutable.Buffer

object Event {

  val eventMap = scala.collection.mutable.HashMap[String, Event]()
  def events = eventMap.values.toVector


  // generate access code
  def GAC : String = {

    var str = alphanumeric.take(Constants.AccessCodeLength).mkString

    // making sure we don't have any duplicate access codes.
    // in the event that most of the possible alphanumeric keys are taken, this takes very long in the worst case,
    // but realistically this will not be a problem

    while (eventMap.keys.exists(_ == str)) {
      str = alphanumeric.take(Constants.AccessCodeLength).mkString
    }

    str
  }

  // does not return an Either because this should not be able to fail ever
  def createEvent(eventName : String) : (String, Event) = {

    // should throw error if eventName is empty (not return an either left, but throw an error)
    val code = GAC
    val event = new Event(eventName, code)
    eventMap += (code -> event)

    println(code)
    (code, event)
  }

}


object participantStatus extends Enumeration {
  type participantStatus = Value
  val free, busy = Value
}
import participantStatus._

class Participant(var user : TGUser, var admin : Boolean = false) {
  var points = 0
  var status : participantStatus = free

  def name = user.name

  def nameWithStatus: String = s"$name ${status2emoji(status)}"

  val status2emoji = Map(
    free -> "😴",
    busy -> "🥵"
  )
}

// The access code doubles as the ID, as they're all unique
class Event(var name: String, val id: String ) {
  
  var participants = Buffer[Participant]()

  var description: String = ""
  var hasStarted = false

  val tasks: Buffer[Task] = Buffer()
  def formatTaskList(list: Buffer[Task]): String = {
    list.map(x => x).mkString("\n")
  }
  def taskList: String = formatTaskList(tasks)

  def tasksByStatus(status: taskStatus): Buffer[Task] = {
    tasks.filter(_.status == status)
  }


  def tasksByRelevance: String = {
    val waiting = tasksByStatus(waitingForMembers)
    val available = tasksByStatus(taskStatus.available)
    val wip = tasksByStatus(workInProgress)
    val complete = tasksByStatus(taskStatus.complete)
    val nA = tasksByStatus(notAvailable)
    formatTaskList(waiting ++  available ++ wip ++ complete ++ nA)
  }

  def addParticipant(participant: Participant): Unit = {
    participants += participant
  }

  def addTask(task: Task): Unit = {
    tasks += task
  }

  def basicInfo : String = {
    name +
    "\n\n" + description 
  }

  override def toString = id + ": "+  name

  def invitation: String = {
    s"Join $name using the code `$id` (Tap to copy!) on @nakkimukibot"
  }

}
