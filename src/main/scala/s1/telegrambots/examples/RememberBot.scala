package s1.telegrambots.examples

import s1.telegrambots.BasicBot

/**
  * Ärsyttävä Botti joka kääntää sanat nurinpäin ja muistelee menneitä
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
object RememberBot extends App {

  val bot = new BasicBot() {

    /**
      * Melkein kaikki järkevä toiminta vaatii, että botin tulee
      * pystyä säilyttämään tilaa. Onneksi Botti on viime kädessä
      * ihan tavallinen olio, jolla saa olla instanssimuuttujia.
      */
    var edellinen = "ei vielä mitään"

    /**
      * Meidän bottimme pystyy muistamaan yhden sanan kerrallaan.
      * Se painaa sanan muistiin tällä metodilla
      *
      * @param msg on telegram-viesti, joka sisältää tietoa
      *            kirjoittajasta, viestin itsensä, jne.
      * @return MErkkijono joka annetaan palautteena kirjoittajalle
      */
    def muista(msg: Message) = {
      // getString antaa telegram-viestin sisältämän tekstin
      edellinen = getString(msg)

      // Vastataan viestin kirjoittajalle jotakin
      "Pistettiin muistiin!"
    }

    /**
      * Vastaavasti botti osaa kertoa muistamansa sanan
      * tällä metodilla
      */
    def kerro(msg: Message) = {
      "Moi " + getUserFirstName(msg) + " sehän oli " + edellinen
    }


    /*
      * Metodilla "command" voidaan omat metodimme: muista ja kerro,
      * asettaa ajettavaksi vain tietyn telegram-komennon yhteydessä.
      *
      * Telegramissa voit siis kirjoittaa esim:
      *
      * /remember Muista käydä kaupassa
      * /recall
      */

    this.command("remember", muista)
    this.command("recall", kerro)

    // Lopuksi Botti pitää vielä saada käyntiin
    this.run()

    println("Started")
  }

}
