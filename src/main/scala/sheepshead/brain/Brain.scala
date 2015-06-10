package sheepshead
package brain

import monocle.macros.Lenses
import net.arya.util.NonEmptySet

import scalaz._, Scalaz._

/**
 * @param c the card that represents the partner
 * @param p
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
  def fold[B](leaster: ⇒ B, picker: Seat ⇒ B) = this match {
    case Yes(seat) ⇒ picker(seat)
    case Leaster ⇒ leaster
  }
}
object Picker {
  case class Yes(seat: Seat) extends Picker
  case object Leaster extends Picker
}


@Lenses
case class Brain(seat: Seat
                ,hand: Hand
                ,pastTricks: List[Trick]
                ,picker: Picker
                ,partnership: Map[Seat, Partnership]
                ,pc: Option[PartnerCard]
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
    sheepshead.wouldLead(hiddenCards(currentTrick))(currentTrick.add(seat, myCard).t)

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
    pc.fold(this) { pc ⇒
      Brain.partnership.modify { partnership ⇒
        import Picker.Yes, Partnership._

        (picker, currentTrick) match {
          case (Yes(pickerSeat), PreviousPlays(trick)) ⇒
            implicit val o = trick.cardOrder
            // if someone played the magic partnership card, who was it? then update the partnership settings
            filterFF(trick.plays.toList)(_._2 === pc.c).map(_._1).headOption.fold(partnership) { pcardPlayer ⇒
              partnership.map {
                case (s, _) ⇒
                  (s,
                    if (seat === s)
                      Self
                    else if ((pickerSeat === seat) === (pcardPlayer === s))
                      Partner
                    else
                      Opponent
                    )
              }
            }
          case _ ⇒ partnership
        }
      }(this)
    }
}
