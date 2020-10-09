package s1.telegrambots.examples

import s1.telegrambots.BasicBot

/**
  * Photobot etsii sanaa ja lähettää löytäessään haluamans kanavalle kuvan
  */
object PhotoBot extends App {

  val bot = new BasicBot() {

    private val pic        = "kuva.png"
    private val wordToFind = "pepper"

    /**
      * Botti vastaa kuvalla löytäessään haluamansa viestin
      */
    def pepperReply(msg: Message) = {
      if (getString(msg).contains(wordToFind))
        sendPhoto(pic, getChatId(msg))
      wordToFind + " found!"
    }

    // Rekisteröidään bat-metodi ajettavaksi aina kun kanavalle kirjoitetaan
    this.anyMessage(pepperReply)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Botti käynnissä")
  }

}
