package s1.telegrambots.nakki
import s1.telegrambots.BasicBot

import scala.collection.mutable.Buffer
import org.jfree.chart.ChartFactory
import org.jfree.chart.ChartUtils
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.renderer.category.BarRenderer
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.axis._
import java.io.File

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
          // add user to users
          if (!TGUser.userExists(userId)) {
            addUser(msg) match {
              case Left(s) =>
                return s
              case Right(s) =>
                additionalText += s
            }
          }
          TGUser.addUserToEventCode(userId, eventId) match {
            case Left(s) => s
            case Right(s) => s
          }
        }
      }
    }

    // Apukomento: Palauttaa käyttäjän aktiivisen tapahtuman
    def currentEvent(msg: Message): Option[Event] = {
      val userID = msg.chat.id
      if (TGUser.userExists(userID)) TGUser.userMap(msg.chat.id).currentEvent else None
    }

    // Apukomento: Paluttaa käyttäjän TGUser-luokan
    def user(msg: Message): Option[TGUser] = {
      TGUser.userMap.get(msg.chat.id)
    }

    // Lisää tehtävän
    def addTask(vector: Vector[String], message: Message): Either[String, String] = {
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
        // Tehtävän kuvaus (oletus "")
        val desc: String = if (vector.length < 4) "" else vector.lift(3).getOrElse("")

        // Uusi tehtävä
        def createTask(event: Event): Unit = {
          val id = event.tasks.lastOption match {
            case Some(t: Task) => t.id + 1
            case _ => 1
          }
          val task = new Task(name, maxPpl, event, id, message)
          task.points = points
          task.description = desc
          event.addTask(task)

          // Tehtävän kuvaus palautukseen
          string = task.toString
        }
        currentEvent(message).foreach(createTask)

        if (string.isEmpty) Left("No active event") else Right("New task (" + string + ") created")
      }
    }

    // Luo uuden tehtävän
    def newTask(msg: Message): String = {
      val args = parseInput(msg, true, true, false)
      args match {
        case Left(s) => s
        case Right(v) =>
          addTask(v, msg) match {
            case Left(s) => s
            case Right(s) => s
          }
      }
    }

    // Palauttaa luettelon kaikista tapahtuman käyttäjistä
    def listUsers(msg: Message): String = {
      currentEvent(msg).foldLeft("List of users:\n")(_ + _.participants.foldLeft("")(_ + _.nameWithStatus + "\n"))
    }

    // Palauttaa luettelon kaikista tapahtuman tehtävistä
    def listTasks(msg: Message): String = {
      currentEvent(msg) match {
        case Some(event) => event.tasksByRelevance match {
          case "" => "No tasks in " + event.name
          case s => s
        }
        case None => "First enter an event"
      }
    }

    // Dibsaa tehtävän (tehtävän numeron perusteella)
    def dibs(vector: Vector[String], message: Message): Either[String, String] = {
      var tasks: Buffer[Task] = currentEvent(message).foldLeft(Buffer[Task]())(_ ++ _.tasks)
      val number = vector.head.toIntOption.getOrElse(-1)

      if (number < 1 || tasks.size < number) Left("Invalid task number")
      else if (user(message).isDefined) {
        tasks.find(_.id == number) match {
          case Some(t: Task) => user(message).get.addTask(t)
          case None => Left("Invalid task number.")
        }
      }
      else Left("Missing user")
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

    def manageTask(msg: Message, action: (Message, Task) => String) = {
      def manage(vector: Vector[String]): Either[String, String] = {
      var tasks: Buffer[Task] = currentEvent(msg).foldLeft(Buffer[Task]())(_ ++ _.tasks)
      val number = vector.head.toIntOption.getOrElse(-1)

      if (number < 1 || tasks.size < number) Left("Invalid task number")
      else if (user(msg).isDefined) {
        tasks.find(_.id == number) match {
          case Some(t: Task) => Right(action(msg, t))
          case None => Left("Invalid task number.")
        }
      }
      else Left("Missing user")
    }


      val args = parseInput(msg, true, false, false)
      args match {
        case Left(s) => s
        case Right(v) =>
          manage(v) match {
            case Left(s) => s
            case Right(s) => s
          }
      }
    }


    def finishTask(msg: Message): String = {
      def _finish(m: Message, t: Task) = user(msg).get.finishTask(t)
      manageTask(msg, _finish)
    }

    def taskInfo(msg: Message): String = {
      def _info(m: Message, t: Task) = {

        s"${t.toString} " +
          s"\n\n " +
          s"${t.description} " +
          s"\n\nWorking:" +
          s"\n${if (t.users.nonEmpty) t.users.map(_.name).mkString(",") else "nobody"}"
      }
      manageTask(msg, _info)
    }



    // Omien tehtävien listaus
    def activeTasks(message: Message): String = {
      user(message).foldLeft("")(_ + _.tasksInEvent.foldLeft("List of tasks")(_ + "\n" + _.toString))
    }

    def invitation(message: Message): String = {

      TGUser.getCurrentEventForUser(message.chat.id) match {
        case Right(Some(e: Event)) => e.invitation
        case Right(None) => "You aren't in any event (no currentEvent)"
        case Left(s) => s
      }
    }

    def eventDescription(msg : Message) : String = {
      TGUser.getCurrentEventForUser(msg.chat.id) match {
        case Right(Some(e : Event)) =>
          e.basicInfo
        case Right(None) =>
          "You don't have an event selected right now."
        case Left(s) =>
          s
      }
    }

    def switchEvent(message: Message): String = {
      val args = parseInput(message, false, false, false)
      args match {
        case Left(s) => s
        case Right(v) => {
          val eventName = v(0)

          val userId = message.chat.id
          val userOption = TGUser.userMap.get(userId)
          if (userOption.isEmpty) return "no user defined"

          val user = userOption.get
          val events = user.events
          val event = events.find(_.name == eventName)
          event match {
            case Some(event) => {
              user.currentEvent = Some(event)
              s"${user.name} switched to ${event.name}"
            }

            case None => s"You haven't joined any event with that name."
          }
        }
      }
    }

    def listEvents(message: Message): String = {
      val userId = message.chat.id
      TGUser.userMap(userId).events.foldLeft("Your events:\n")(_ + _.name + "\n")
    }

    def createPointsStats(message: Message): String = {
      currentEvent(message) match {
        case None => "No event - cannot load points stats."
        case Some(event) => {
          this.writeToChat("Loading points stats...", message.chat.id)
          val dcd = new DefaultCategoryDataset
          event.participants.foreach(participant => dcd.setValue(participant.points, "", participant.user.name))
          val chart = ChartFactory.createBarChart("Nakkipisteet","Name", "Points", dcd, PlotOrientation.VERTICAL, true, true, false)
          val categoryPlot = chart.getCategoryPlot
          val yAxis = categoryPlot.getRangeAxis
          yAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
          val br = categoryPlot.getRenderer.asInstanceOf[BarRenderer]
          br.setMaximumBarWidth(.20)
          val image = new File("pointsstats.png")
          ChartUtils.saveChartAsPNG(image, chart, 500, 500)
          this.sendPhoto(image.getPath, message.chat.id)
          Thread.sleep(3000)
          if (image.exists) image.delete()
          "Points stats created."
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
        "/switch [event name] to work in one of your other events\n"+
        "/invitation to create an invitation message\n"+
        "\nTasks:\n" +
        "/newtask [task name] (max number of people) (points)\n"+
        "/tasklist List of tasks in the active event\n"+
        "/dibs [task number] to pick up a task\n"+
        "/finish [task number] to finish a task\n"+
        "/info [task number] to get task description\n"+
        "/problem [task number] to report a problem with a task (not implemented)\n"+
        "/mytasks List of active tasks\n"+
        "\nUsers:\n" +
        "/userlist List of users in the active event"
    }

    // Yleiset
    this.command("start", startMessage)
    this.command("help", helpMessage)

    // Tapahtumat
    this.command("newevent", createEvent)
    this.command("join", joinEvent)
    this.command("invitation", invitation)
    this.command("switch", switchEvent)
    this.command("eventlist", listEvents)

    // Tehtävät
    this.command("newtask", newTask)
    this.command("tasklist", listTasks)
    this.command("dibs", joinTask)
    this.command("finish", finishTask)
    this.command("info", taskInfo)
    this.command("mytasks", activeTasks)

    // Käyttäjät
    this.command("userlist", listUsers)
    this.command("pointsstats", createPointsStats)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}

