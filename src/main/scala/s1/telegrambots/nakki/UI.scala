package s1.telegrambots.nakki
import s1.telegrambots.BasicBot
import com.bot4s.telegram.models.Message

import collection.mutable.{Buffer, Map}

object UI extends App {

  val bot = new BasicBot() {

    def parseInput(msg : Message, spacesAllowed : Boolean, specialCharAllowed : Boolean, emptyArgsAllowed : Boolean) : Either[Vector[String], String] = {
      val retVec = getString(msg).split(Constants.argSeparator).map(_.trim()).toVector

      if (!spacesAllowed) {
        if (!retVec.forall(_.forall(!_.isSpaceChar))) return Right("Spaces are not allowed in this input")
      }

      if (!specialCharAllowed) {
        if (!retVec.forall(_.forall(c => c.isLetterOrDigit || c.isWhitespace))) return Right("You can only use letters, numbers, and whitespace characters in this input")
      }

      if (!emptyArgsAllowed) {
        if (!retVec.forall(!_.isEmpty())) return Right("Empty arguments are not allowed in this input")
      }

      Left(retVec)
    }

    def createEvent(msg: Message): String = {
      val args = parseInput(msg, true, false, false)
      
      args match {
        case Right(s) =>
          return s
        case Left(v) =>
          val eventName = v(0)
          val (code, e) = Event.createEvent(eventName)

          // add user to users
          val addUserRep = addUser(msg)
          
          // add user to event
          addUserToEvent(msg, e)

          "You have now created and entered " + eventName +
          "\nInvite others using the access code: " + e.id
      }
    }

    def addUser(msg : Message) : String = {
      val id = msg.chat.id
      val name = msg.from.map(_.firstName).getOrElse("").filterNot(_.isWhitespace)

      if (name.isEmpty) {
        "Username cannot be empty"
      } else {
        TGUser.addUser(id, name)
      }
    }

    def addUserToEvent(msg : Message, event : Event) : String = {
      val id = msg.chat.id

      TGUser.addUserToEvent(id, event)
    }

    def createTask(message: Message): String = {
      if (currentEvent(message).isEmpty) return "Enter or create an event to create tasks"
      val input = getString(message).split("[,\n]").map(_.trim)
      val name = input(0)
      val maxPpl = input(1)
      val event = currentEvent(message).get
      val t = new Task(name, maxPpl.toInt, event)
      event.tasks += t
      t.toString
    }

    def listTasks(message: Message): String = {
      if (currentEvent(message).isEmpty) "Enter or create an event to list tasks"
      else currentEvent(message).get.taskList
    }

    def startMessage(message: Message) = {
      "Welcome to Nakkibotti!\n"+
        "/newevent [event name] to create a new event\n"+
        "/join [invite code] to join an event\n"+
        "/help for all commands"
    }

    this.command("createevent", createEvent)
    this.command("createtask", createTask)
    this.command("tasks", listTasks)
    this.command("start", startMessage)


    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}

