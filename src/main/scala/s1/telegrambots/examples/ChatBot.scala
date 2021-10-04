package s1.telegrambots.examples

import com.bot4s.telegram.models.{ChatId, Message}

import s1.telegrambots.BasicBot

import scala.collection.mutable.{Buffer, Map}
import scala.util.Random

/**
  * Example bot that remembers the discussion it has with any user.
  * Should allow simultaneous discussions where the bot remembers what we were talking about.
  */
object ChatBot extends App {

  /**
    * This class saves a discussion with someone.
    *
    * * userInputs are recorded automatically and you can check them if you want
    * * bots inputs are recorded by calling rememberText
    * * the "action" we are performing can be recorded with rememberAction. This can be used to for example ask the user something and when they answer, know what they answered to.
    * * finally lastAction gives the last action we performed or "started"
    */
  class Discussion {
    val userInputs                = Buffer[String]()
    val botInputs                 = Buffer[String]()
    var ourAction:Option[String]  = None

    def rememberAction(action: String) = {
      this.ourAction = Some(action)
    }

    def rememberText(text: String) = {
      this.botInputs += text
    }

    def lastAction = {
      this.ourAction.getOrElse("started")
    }
  }

  /**
    * This class stores all discussions. Any discussion can be fetched with a message that holds a chat id.
    */

  object Discussions {
    val discussions = Map[ChatId, Discussion]()

    /**
      * Returns a discussion based on a chat id inside any message
      * @param msg message used to query the list of discussions
      * @return the discussion related to the message
      */
    def getDiscussion(msg: Message) = this.discussions.getOrElseUpdate(msg.chat.id, new Discussion)

    /**
      * Used to automatically log the users messages in each discussion
      * @param msg message to log
      */
    def rememberChats(msg: Message): Unit = {
      val userText = msg.text.getOrElse("no text (image or something else)")
      this.getDiscussion(msg).userInputs += userText
    }
  }



  /** Chat bot that uses the classes above */
  val bot = new BasicBot() {

    // Four discussion topics
    private val topics = Buffer("birds", "cats", "dogs", "fish")

    // This method is always called when something happens in the chat
    def askAndRemember(msg: Message) = {

      // get the corresponding discussion
      val discussion      = Discussions.getDiscussion(msg)

      // Check what we last asked about and how the user replied
      val previouslyAsked = discussion.lastAction
      val userWrote       = getString(msg)

      // Select a new topic and question
      val nextTopic       = topics(Random.nextInt(topics.length))
      val nextQuestion    = s"What do you think about $nextTopic ?"

      // save our topic and reply in the discussion (they are handy when we get the users reply)
      discussion.rememberAction(nextTopic)
      discussion.rememberText(nextQuestion)

      println("log: user wrote    - " + userWrote)
      println("log: we reply      - " + nextQuestion)
      println("log: our topic was - " + nextTopic)
      println("")

      // Finally, create our reply from the texts above
      s"I asked you about $previouslyAsked and you said: $userWrote. $nextQuestion"
    }

    // rememberChats and askAndRemember both need to be called for every message in the chat
    this.anyMessage(Discussions.rememberChats)
    this.replyToMessage(askAndRemember)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}
