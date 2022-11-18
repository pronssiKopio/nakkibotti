package s1.telegrambots.nakki
import scala.collection.mutable.{HashMap, Buffer}


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

  def userExists(id : Long) : Boolean = {
    userMap.contains(id)
  }

}

class TGUser(val telegramId: Long, var name: String) {
  var events = Buffer[Event]()
  var currentEvent : Option[Event] = None

  // Adds user, if it is not already in that event
  def addEvent(event : Event) : Either[String, String] = {
    if (events.contains(event)) {
      Left("User is already in that event")
    } else {
      events += event
      currentEvent = Some(event)

      Right(s"Succesfully added ${name} to " + event.name)
    }
  }

}
