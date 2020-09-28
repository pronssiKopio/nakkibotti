package s1.telegrambots.examples

import s1.telegrambots.BasicBot

/**
  * Tervehdysbotti, joka tervehtii kanavalle kirjoittajia
  *
  * Botti saamaan reagoimaan kanavan tapahtumiin luomalla funktio/metodi joka käsittelee
  * halutuntyyppistä dataa ja joko palauttaa merkkijonon tai tekee jotain muuta saamallaan
  * datalla.
  *
  */
object GreetBot2 extends App {

  val bot = new BasicBot() {

    /**
      * Muokkaa käyttäjän kirjoittamasta viestistä viestin kanavalle
      *
      */
    def moikkaa(message: Message): String = {

      message.from match {
        case Some(kayttaja) => "Terve taas " + kayttaja.firstName
        case None => "Ohhoh... tuntematon puhuja"
      }
    }

    /**
      * rekisteröi botille uuden toiminnon joka ajetaan aina kun
      * kanavalle tulee uusi viesti. Toiminnon tulee ottaa parametrina
      * message ja palauttaa String
      */
    this.replyToMessage(moikkaa)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

  //synchronized{wait()}
}
