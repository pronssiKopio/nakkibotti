package s1.telegrambots.nakki
import s1.telegrambots.BasicBot


object UI extends App {

  val bot = new BasicBot() {


    // Right has the desired return, left has an error message.
    def parseInput(msg : Message, spacesAllowed : Boolean, specialCharAllowed : Boolean, emptyArgsAllowed : Boolean) : Either[String, Vector[String]] = {
      val retVec = getString(msg).split(Constants.argSeparator).map(_.trim()).toVector

      // checking for any spaces
      if (!spacesAllowed) {
        if (!retVec.forall(_.forall(!_.isSpaceChar))) return Left("Spaces are not allowed in this input")
      }

      // only letters, numbers, and whitespace
      if (!specialCharAllowed) {
        if (!retVec.forall(_.forall(c => c.isLetterOrDigit || c.isWhitespace))) return Left("You can only use letters, numbers, and whitespace characters in this input")
      }

      // whether empty arguments are allowed
      if (!emptyArgsAllowed) {
        if (!retVec.forall(_.nonEmpty)) return Left("Empty arguments are not allowed in this input")
      }

      Right(retVec)
    }

    def createEvent(msg: Message): String = {
      val args = parseInput(msg, true, false, false)
      
      args match {
        case Left(s) =>
          s
        case Right(v) =>
          val eventName = v(0)
          val (code, e) = Event.createEvent(eventName)

          var additionalText = ""
          
          // add user to users
          if (!TGUser.userExists(msg.chat.id)) {
            addUser(msg) match {
              case Left(s) =>
                return s
              case Right(s) =>
                additionalText += s
            }
          }
          
          // add user to event
          addUserToEvent(msg, e) match {
            case Left(s) =>
              return s
            case Right(s) =>
              additionalText += s"\n$s"
          }

          "You have succesfully created " + eventName +
          "\nInvite others using the access code: " + e.id +
          s"\n${additionalText}"
      }
    }

    def addUser(msg : Message) : Either[String, String] = {
      val id = msg.chat.id
      val name = msg.from.map(_.firstName).getOrElse("").filterNot(_.isWhitespace)

      if (name.isEmpty) {
        Left("Username cannot be empty")
      } else {
        TGUser.addUser(id, name)
      }
    }

    def addUserToEvent(msg : Message, event : Event) : Either[String, String] = {
      val id = msg.chat.id

      TGUser.addUserToEvent(id, event)
    }

    def joinEvent(msg: Message) : String = {
      val args = parseInput(msg, false, false, false)
      args match {
        case Left(s) => s
        case Right(v) => {
          val eventId = v(0)
          val userId = msg.chat.id

          var additionalText = ""
          println(1)
          // add user to users
          if (!TGUser.userExists(userId)) {
            addUser(msg) match {
              case Left(s) =>
                return s
              case Right(s) =>
                additionalText += s
            }
          }
          println(2)
          TGUser.addUserToEventCode(userId, eventId) match {
            case Left(s) => s
            case Right(s) => s
          }
        }
      }
    }

    def startMessage(message: Message) = {
      "Welcome to Nakkibotti!\n"+
        "/newevent [event name] to create a new event\n"+
        "/join [invite code] to join an event\n"+
        "/help for all commands"
    }

    this.command("newevent", createEvent)
    this.command("start", startMessage)
    this.command("join", joinEvent)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}

