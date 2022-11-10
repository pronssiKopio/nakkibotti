package s1.telegrambots.nakki
import s1.telegrambots.BasicBot

import collection.mutable.Buffer
import collection.mutable.Map

object UI extends App {

  val events = Buffer[Event]()

  /**  Listataaan, mikä tapahtuma käyttäjällä on aktiivisena, ettei joka komennon yhteydessä tarvitse kertoa tapahtumaa*/
  val inEvent: Map[Long, Option[Event]] = Map()

  val bot = new BasicBot() {
    private def currentEvent(message: Message) = inEvent(message.from.get.id)


    def createEvent(message: Message): String = {
      val name = getString(message)
      val e = new Event(name, events.size)
      val u = new User(message.from.get.id.toString, message.from.get.firstName, e, true)
      inEvent += message.from.get.id -> Some(e)
      events += e

      "You have now created and entered " + name +
      "\nInvite others using the access code: " + e.accessCode
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

    this.command("createevent", createEvent)
    this.command("createtask", createTask)
    this.command("tasks", listTasks)


    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}

