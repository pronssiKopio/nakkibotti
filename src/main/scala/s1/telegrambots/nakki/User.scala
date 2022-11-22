package s1.telegrambots.nakki
import scala.collection.mutable
import scala.collection.mutable.{Buffer, HashMap}


object TGUser {
  val userMap = HashMap[Long, TGUser]()
  def users = userMap.values.toVector

  def addUser(id : Long, name : String) : Either[String, String] = {

    if (userMap.contains(id)) {
      Left("User already exists")
    } else {
      userMap += (id -> new TGUser(id, name))
      Right(s"Succesfully added user ${name}!")
    }
  }

  def addUserToEvent(id : Long, event : Event) : Either[String, String] = {
    userMap.get(id) match {
      case None =>
        Left("User does not exist")
      case Some(u) =>
        u.addEvent(event)
    }
  }

  def addUserToEventCode(id : Long, eventCode : String) : Either[String, String] = {
    Event.eventMap.get(eventCode) match {
      case None => {
        Left("The event with this code does not exist")
      }
      case Some(e) => {
        addUserToEvent(id, Event.eventMap(eventCode))
      }
    }
  }

  def addUserToTask(id : Long, task: Task) : Either[String, String] = {
    userMap.get(id) match {
      case None =>
        Left("User does not exist")
      case Some(u) =>
        u.addTask(task)
    }
  }

  def userExists(id : Long) : Boolean = {
    userMap.contains(id)
  }

}

class TGUser(val telegramId: Long, var name: String) {
  var events = Buffer[Event]()
  var tasks = Buffer[Task]()

  var currentEvent : Option[Event] = None

  // Palauttaa käyttäjää vastaavan Participant-luokan halutusta tapahtumasta
  def participant(event: Event): Option[Participant] = {
    event.participants.find(_.user == this)
  }

  // Adds user, if it is not already in that event
  def addEvent(event : Event) : Either[String, String] = {
    if (events.contains(event)) {
      Left("User is already in that event")
    } else {
      events += event
      currentEvent = Some(event)

      // Lisää käyttäjän tapahtuman käyttäjälistaan
      event.addParticipant(new Participant(this, false))

      Right(s"Succesfully added ${name} to " + event.name)
    }
  }

  // Adds user, if it is not already in that task
  // Tämä metodi täytyy korjata tomimaan ilman nykyisenkaltaista currentParticipantia
  def addTask(task: Task, event: Event) : Either[String, String] = {
    if (tasks.contains(task)) {
      Left("User is already in that task")
    } else {
      tasks += task
//      currentTask = Some(task)

      // Lisää käyttäjän tehtävän käyttäjälistaan
      participant(event).foreach(task.addUser)

      Right(s"Succesfully added ${name} to " + task.name)
    }
  }

  def addTask(task: Task) : Either[String, String] = {
    if (currentEvent.isDefined) {
      addTask(task, currentEvent.get)
    }
    else {
      Left("No active event found")
    }
  }

}
