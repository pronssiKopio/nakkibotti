package s1.telegrambots.examples

import s1.telegrambots.BasicBot

/**
  * Ärsyttävä Botti joka kääntää sanat nurinpäin
  *
  * Botti saamaan reagoimaan kanavan tapahtumiin luomalla funktio/metodi joka käsittelee
  * halutuntyyppistä dataa ja joko palauttaa merkkijonon tai tekee jotain muuta saamallaan
  * datalla.
  *
  * Alla on yksinkertainen esimerkki - metodi joka ottaa merkkijonon ja kääntää sen nurin.
  * Luokassa BasicBot on joukko metodeja, joilla voit asettaa botin suorittamaan oman metodisi
  * ja tekemään tiedolla jotain. replyToString rekisteröi bottiin funktion joka saa
  * syötteekseen jokaisen kanavalle kirjoitetun merkkijonon. Se, mitä funktio
  * palauttaa lähetetään kanavalle.
  */
object ReverseBot extends App {

  val bot = new BasicBot() {


    /**
      * Kääntää sanan toisin päin
      */
    def nurinpain(s: String) = s.reverse

    /**
      * rekisteröi botille uuden toiminnon joka ajetaan aina kun
      * kanavalle kirjoitetaan jotain.
      */
    this.replyToString(nurinpain)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }


  //synchronized{wait()}
}
