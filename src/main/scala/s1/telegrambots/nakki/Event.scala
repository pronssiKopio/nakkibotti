package s1.telegrambots.nakki

import util.Random.alphanumeric
import collection.mutable.Buffer

class Event(var name: String, id: Int ) {
  
  val accessCode = alphanumeric.take(Constants.AccessCodeLength).mkString
  var description: String = ""

  val tasks: Buffer[Task] = Buffer()
  def taskList: String =
    {
      val indices = 1 to tasks.size
      (indices zip tasks).map(x => x._1 + " " + x._2).mkString("\n")
    }

  override def toString = id + ": "+  name

}
