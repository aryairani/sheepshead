package sheepshead
package brain

import monocle.macros.Lenses
import net.arya.util.NonEmptySet
import net.arya.util.filter.filterFF
import net.arya.util.unsafe.atRandom1

import scalaz._, Scalaz._

/**
 * @param c the card that represents the partner
 */
final case class PartnerCard(c: Card)

sealed trait Partnership
object Partnership {
  case object Self extends Partnership
  case object Partner extends Partnership
  case object Opponent extends Partnership
  case object Unknown extends Partnership
}

sealed trait Picker extends Product with Serializable {
  import Picker._
  def fold[B](leaster: ⇒ B, picker: (Seat,Card) ⇒ B) = this match {
    case PickerCard(seat, card) ⇒ picker(seat, card)
    case Leaster ⇒ leaster
  }
}
object Picker {
  case class PickerCard(seat: Seat, partnerCard: Card) extends Picker
  case object Leaster extends Picker
}


@Lenses
case class Brain(mySeat: Seat
                 ,hand: Hand
                 ,pastTricks: List[Trick]
                 ,picker: Picker
                 ,partnership: Map[Seat, Partnership]
//                 ,seenBlind: Option[(Card, Card)]
                  ) { self ⇒

  def points(pastTricks: List[Trick])(seat: Seat): Int =
    pastTricks.filter(_.leadingSeat === seat).foldMap(_.points)

  /**
   * Which cards from your hand would lead the current trick?
   */
  def handCardsWhichWouldLead(currentTrick: CurrentTrick): Set[Card] =
    currentTrick.toOption map
      sheepshead.wouldLead(hand.toSet) getOrElse
      hand.toSet

  /**
   * Which cards not yet seen in play (hidden cards)
   *  would lead the current trick? or None, if the
   *  suit hasn't yet been established.
   */
  def hiddenCardsWhichWouldLead(currentTrick: CurrentTrick): Option[Set[Card]] =
    currentTrick.toOption map
      sheepshead.wouldLead(hiddenCards(currentTrick))

  /**
   * Assuming I play a certain card on the current Trick,
   * which hidden cards could beat it?
   */
  def whichHiddenCouldBeat(currentTrick: CurrentTrick)(myCard: Card): Set[Card] =
    sheepshead.wouldLead(hiddenCards(currentTrick))(currentTrick.add(mySeat, myCard).t)

  def hiddenCards(currentTrick: CurrentTrick): Set[Card] = (
    fulldeck
      -- hand.toSet
      -- currentTrick.fold(Nil, _.playCards.list)
      -- pastTricks.map(_.playCards.list).flatten
    )

  /**
   * Return a legal play at random from the current hand.
   * "Unsafe" due to side-effecting RNG.
   */
  def unsafeRandomLegal(currentTrick: CurrentTrick): Card = {
    currentTrick.toOption
      .map(_.ssuit)
      .map(filterLegal(hand))
      .getOrElse(hand) |> atRandom1[NonEmptySet, Card]
  }

  // todo: test this
  def updatedPartnerships(currentTrick: CurrentTrick): Brain =
    currentTrick.fold(this, updatedPartnerships)


  def updatedPartnerships(trick: Trick): Brain =

    picker.fold(this, (pickerSeat, pc) ⇒
      Brain.partnership.modify { priorPartnershipLookup ⇒

        import Partnership._
        implicit val o = trick.cardOrder

        // if someone played the magic partnership card, who was it? then update the partnership settings
        filterFF(trick.plays.toList)(_._2 === pc) // look for any play pairs matching the magic partnership card
          .headOption // there should be zero or one
          .map(_._1) // we'll care about the first element of the pair: the seat
          // if there was no match, just stick with the old partnership lookup,
          .fold(priorPartnershipLookup) { pcardPlayer ⇒ // else, knowing the seat of the partnership card player,
          priorPartnershipLookup.map {
            // go through all the players in the partnership lookup map
            case (playerSeat, _) ⇒ // given the seat,
              val thisPersonIsMy =
                if (playerSeat === mySeat)
                  Self
                else if ((pickerSeat === mySeat) === (pcardPlayer === playerSeat))
                  Partner
                else
                  Opponent

              (playerSeat, thisPersonIsMy) // replace the map entry with one mapping
            // the player to the updated partnership relationship

          }
        }
      }(this)
    )

//  def addBestFromBlind(blind: (Card, Card)): Brain = {
//    val newHand = hand insert blind._1 insert blind._2
//    new Brain(mySeat, newHand, pastTricks, picker, partnership) {
//      override val
//    }
//  }
}
