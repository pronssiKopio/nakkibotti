package s1.telegrambots.nakki
import scala.collection.mutable
import scala.collection.mutable.{Buffer, HashMap}


object TGUser {
  val userMap = HashMap[Long, TGUser]()
  def users = userMap.values.toVector

  def getCurrentEventForUser(id: Long): Either[String, Option[Event]] = {
    if (userMap.contains(id)) Right(userMap(id).currentEvent)
    else Left("You haven't joined any events")
  }

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
  var currentTask : Option[Task] = None
  var currentParticipant: Option[Participant] = None

  // Adds user, if it is not already in that event
  def addEvent(event : Event) : Either[String, String] = {
    if (events.contains(event)) {
      Left("User is already in that event")
    } else {
      events += event
      currentEvent = Some(event)
      currentParticipant = Some(new Participant(this, false))

      // Lisää käyttäjän tapahtuman käyttäjälistaan
      currentParticipant.foreach(event.addParticipant)

      Right(s"Succesfully added ${name} to " + event.name)
    }
  }

  // Adds user, if it is not already in that event
  def addTask(task: Task) : Either[String, String] = {
    if (tasks.contains(task)) {
      Left("User is already in that task")
    } else {
      tasks += task
      currentTask = Some(task)

      // Lisää käyttäjän tehtävän käyttäjälistaan
      currentParticipant.foreach(task.addUser)

      Right(s"Succesfully added ${name} to " + task.name)
    }
  }

}
