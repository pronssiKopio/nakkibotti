package s1.telegrambots.nakki
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
  def addTask(task: Task, event: Event) : Either[String, String] = {
    if (task.users.exists(_.user == this)) {
      Left("User is already in that task")
    } else {

      // Lisää käyttäjän tehtävän käyttäjälistaan
      participant(event) match {
        case Some(p: Participant) => {
          if (task.addUser(p)) Right(s"Succesfully added ${name} to " + task.name)
          else Left(s"${name} is already busy")
        }
        case None => Left(s"$name is not a participant in $event")
      }


    }
  }

  // Sama kuin ylempi, mutta ottaa aina aktiivisen tapahtuman
  def addTask(task: Task) : Either[String, String] = {
    if (currentEvent.isDefined) {
      addTask(task, currentEvent.get)
    }
    else {
      Left("No active event found")
    }
  }

  // Luettelo tehtävistä tapahtumassa
  def tasksInEvent(event: Event): Buffer[Task] = {
    event.tasks.filter(_.users.map(_.user).contains(this))
  }

  // Sama kuin ylempi, mutta ottaa aina aktiivisen tapahtuman
  def tasksInEvent: Buffer[Task] = {
    currentEvent.foldLeft(Buffer[Task]())(_ ++ tasksInEvent(_))
  }

  def finishTask(task: Task): String = {
    task.finish()
    s"${task.name} finished"
  }

}
