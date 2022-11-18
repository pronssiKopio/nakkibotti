package s1.telegrambots.nakki
import s1.telegrambots.BasicBot

import scala.collection.mutable.Buffer


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

    // Lisää tehtävän
    def addTask(vector: Vector[String]): Either[String, String] = {
      if (vector.size < 1)
        Left("Missing arguments")
      else {
        var string = ""

        // Tehtävän nimi
        val name = vector.head
        // Tehtävään tarvittavien määrä (oletus 1)
        val maxPpl = if (vector.length < 2) 1 else vector(1).toIntOption.getOrElse(1)
        // Tehtävän pistemäärä (oletus 1)
        val points = if (vector.length < 3) 1 else vector(2).toIntOption.getOrElse(1)

        // Uusi tehtävä
        def createTask(event: Event): Unit = {
          val task = new Task(name, maxPpl, event)
          task.points = points
          event.addTask(task)

          // Tehtävän kuvaus palautukseen
          string = task.toString
        }
        Event.currentEvent.foreach(createTask)

        if (string.isEmpty) Left("No active event") else Right("New task (" + string + ") created")
      }
    }

    // Luo uuden tehtävän
    def newTask(msg: Message): String = {
      val args = parseInput(msg, true, true, false)
      args match {
        case Left(s) => s
        case Right(v) =>
          addTask(v) match {
            case Left(s) => s
            case Right(s) => s
          }
      }
    }

    // Palauttaa luettelon kaikista tapahtuman käyttäjistä
    def listUsers(msg: Message): String = {
      Event.currentEvent.foldLeft("List of users:\n")(_ + _.participants.foldLeft("")(_ + _.user.name + "\n"))
    }

    // Palauttaa luettelon kaikista tapahtuman tehtävistä
    def listTasks(msg: Message): String = {
      Event.currentEvent.foldLeft("List of tasks:\n")(_ + _.tasks.foldLeft("")(_ + _.name + "\n"))
    }

    // Dibsaa tehtävän (tehtävän numeron perusteella)
    def dibs(vector: Vector[String], message: Message): Either[String, String] = {
      var tasks: Buffer[Task] = Event.currentEvent.foldLeft(Buffer[Task]())(_ ++ _.tasks)
      val number = vector.head.toIntOption.getOrElse(-1)
      val user = TGUser.userMap(message.chat.id)

      if (number < 1 || tasks.size < number) Left("Invalid task number")
      else user.addTask(tasks(number - 1))
    }

    // Tehtävään liittyminen
    def joinTask(msg: Message): String = {
      val args = parseInput(msg, true, false, false)
      args match {
        case Left(s) => s
        case Right(v) =>
          dibs(v, msg) match {
            case Left(s) => s
            case Right(s) => s
          }
      }
    }

    def startMessage(message: Message) = {
      "Welcome to Nakkibotti!\n"+
        "/newevent [event name] to create a new event\n"+
        "/join [invite code] to join an event\n"+
        "/help for all commands"
    }

    def helpMessage(message: Message) = {
      "Commands:\n"+
        "General:\n"+
        "/help for all commands\n" +
        "/start Starts the bot\n" +
        "\nEvents:\n" +
        "/newevent [event name] to create a new event\n"+
        "/join [invite code] to join an event\n"+
        "\nTasks:\n" +
        "/newtask [task name] (max number of people) (points)\n"+
        "/tasklist List of tasks in the active event\n"+
        "/dibs Dibs task\n"+
        "\nUsers:\n" +
        "/userlist List of users in the active event"
    }

    // Yleiset
    this.command("start", startMessage)
    this.command("help", helpMessage)

    // Tapahtumat
    this.command("newevent", createEvent)
    this.command("join", joinEvent)

    // Tehtävät
    this.command("newtask", newTask)
    this.command("tasklist", listTasks)
    this.command("dibs", joinTask)

    // Käyttäjät
    this.command("userlist", listUsers)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}

