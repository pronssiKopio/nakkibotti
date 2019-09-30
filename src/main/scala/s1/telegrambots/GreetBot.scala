package s1.telegrambots

import info.mukel.telegrambot4s._
import api._
import methods.{SendMessage, _}
import models.{InlineKeyboardButton, InlineKeyboardMarkup, _}
import declarative._

/**
 * Tervehdysbotti, joka tervehtii kanavalle tulijoita
 *
 * Botti saamaan reagoimaan kanavan tapahtumiin luomalla funktio/metodi joka käsittelee
 * halutuntyyppistä dataa ja joko palauttaa merkkijonon tai tekee jotain muuta saamallaan
 * datalla.
 *
 */
object GreetBot extends App {

   val bot =  new BasicBot() {

     /**
      * Luo käyttäjän nimestä tervehdysviestin
      */
     def tervehdi(kayttaja: User) = "Moikka "+kayttaja.firstName

     /**
      * rekisteröi botille uuden toiminnon joka ajetaan aina kun
      * kanavalle tulee uusi käyttäjä.
      */
     this.joinMessage(tervehdi)

     // Lopuksi Botti pitää vielä saada käyntiin
     this.run()

     println("Started")
   }

   //synchronized{wait()}
}
