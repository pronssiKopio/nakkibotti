package s1.telegrambots.examples

import com.bot4s.telegram.models.User
import s1.telegrambots.BasicBot

/**
  * Tervehdysbotti, joka tervehtii kanavalle tulijoita
  *
  * Botti saamaan reagoimaan kanavan tapahtumiin luomalla funktio/metodi joka käsittelee
  * halutuntyyppistä dataa ja joko palauttaa merkkijonon tai tekee jotain muuta saamallaan
  * datalla.
  *
  */
object GreetBot extends App {

  val bot = new BasicBot() {

    /**
      * Luo käyttäjän nimestä tervehdysviestin
      */
    def tervehdi(kayttaja: User) = "Moikka " + kayttaja.firstName

    /**
      * rekisteröi botille uuden toiminnon joka ajetaan aina kun
      * kanavalle tulee uusi käyttäjä. Toiminnon tulee ottaa parametrina
      * User ja palauttaa String
      */
    this.joinMessage(tervehdi)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

  //synchronized{wait()}
}
