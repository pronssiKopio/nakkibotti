package s1.telegrambots.nakki
import scala.collection.mutable.HashMap
import com.bot4s.telegram.models.Message


object TGUser {
  val userMap = HashMap[Long, TGUser]()
  def users = userMap.values.toVector

  def addUser(id : Long, name : String) : String = {

    if (userMap.get(id).isDefined) {
      "User already exists"
    } else {
      userMap += (id -> new TGUser(id, name))
      "Succesfully added user!"
    }
  }

  def addUserToEvent(id : Long, event : Event) : String = {
    userMap.get(id) match {
      case None =>
        "User does not exist"
      case Some(u) =>
        u.addEvent(event)
    }
  }

  def addUserToEventCode(id : Long, eventCode : String) = {
    ???
  }

}

class TGUser(val telegramId: Long, var name: String) {
  var events = Vector[Event]()
  var currentEvent : Option[Event] = None

  def addEvent(event : Event) : String = {
    if (events.contains(event)) {
      "User is already in that event"
    } else {
      events = events :+ event
      currentEvent = Some(event)

      "Succesfully added user to event " + event.name
    }
  }

}
