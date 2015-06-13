package sheepshead
package util

import sheepshead.brain.{PreviousPlays, YourLead, CurrentTrick}

import scalaz._, Scalaz._

import net.arya.util.unsafe.{unsafeListToNel, unsafeListToNes}

object CardParser {
  def parseTrick(nextSeat: Seat)(str: String): Option[CurrentTrick] =
    if (str == "") Some(YourLead)
    else
      str.split(' ') // first card played is on the left
        .toList.reverse // first card played is at the tail
        .map(CardParser.simpleCard)
        .sequence[Option,Card]
        .map { cards ⇒
        PreviousPlays(Trick(cards.last.ssuit, unsafeListToNel(Seat.seatList(nextSeat) zip cards))) // fixme
      }

  def parseTrickMandatory(nextSeat: Seat)(str: String): Option[PreviousPlays] =
    if (str == "") None
    else
      str.split(' ') // first card played is on the left
        .toList.reverse // first card played is at the tail
        .map(CardParser.simpleCard)
        .sequence[Option,Card]
        .map { cards ⇒
        PreviousPlays(Trick(cards.last.ssuit, unsafeListToNel(Seat.seatList(nextSeat) zip cards))) // fixme
      }

  def parseHand(str: String): Option[Hand] =
    if (str == "") None
    else
      str.split(' ').toList
        .map(CardParser.simpleCard)
        .sequence[Option,Card]
        .map(unsafeListToNes)

  def simpleRank(s: String): Option[Rank] = {
    import Rank._
    s.toLowerCase match {
      case "a" | "ace" ⇒ A.some
      case "k" | "king" ⇒ K.some
      case "q" | "queen" ⇒ Q.some
      case "j" | "jack" ⇒ J.some
      case "10" | "t" ⇒ _10.some
      case "9" | "n" ⇒ _9.some
      case "8" | "e" ⇒ _8.some
      case "7" | "s" ⇒ _7.some
      case err ⇒ none
    }
  }
  def simpleSuit(s: String): Option[Suit] = {
    import Suit._
    s.toLowerCase match {
      case "c"|"club"|"clubs"|"♣" ⇒ club.some
      case "s"|"spade"|"spades"|"♠" ⇒ spade.some
      case "h"|"heart"|"hearts"|"♥" ⇒ heart.some
      case "d"|"diamond"|"diamonds"|"♦" ⇒ diamond.some
      case err ⇒ none
    }
  }
  def simpleCard(s: String): Option[Card] = {
    s.split('.') match {
      case Array(rank, suit) ⇒
        (simpleRank(rank) |@| simpleSuit(suit))(Card.apply)
      case _ ⇒ none
    }
  }
}
