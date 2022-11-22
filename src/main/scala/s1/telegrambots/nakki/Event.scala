package s1.telegrambots.nakki

import util.Random.alphanumeric
import collection.mutable.Buffer

object Event {

  val eventMap = scala.collection.mutable.HashMap[String, Event]()
  def events = eventMap.values.toVector

  // Tallettaa aktiivisen tapahtuman
  // Tämä ei toimi, koska tapahtuman pitää riippua käyttäjästä (tässä oletetaan, että tapahtuma on kaikille käyttäjille sama)
  // var currentEvent: Option[Event] = None


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


object participantState extends Enumeration {
  type participantState = Value
  val free, busy = Value
}
import participantState._

class Participant(var user : TGUser, var admin : Boolean = false) {
  var points = 0
  var state : participantState = free
}

// The access code doubles as the ID, as they're all unique
class Event(var name: String, val id: String ) {
  
  var participants = Buffer[Participant]()

  var description: String = ""
  var hasStarted = false

  val tasks: Buffer[Task] = Buffer()
  def taskList: String =
    {
      val indices = 1 to tasks.size
      (indices zip tasks).map(x => x._1 + " " + x._2).mkString("\n")
    }

  def addParticipant(participant: Participant): Unit = {
    participants += participant
  }

  def addTask(task: Task): Unit = {
    tasks += task
  }

  override def toString = id + ": "+  name

}
