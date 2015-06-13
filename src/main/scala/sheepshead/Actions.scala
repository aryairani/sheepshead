package sheepshead

import net.arya.util.{EmptySet, NonEmptySet}
import net.arya.util.filter._
import sheepshead.brain._

import scalaz._, Scalaz._, Free.FreeC, effect.IO

//object ConsoleAction extends (Action ~> IO) {
//
//  def retryReadTrick: IO[CurrentTrick] =
//    IO.readLn.flatMap(s ⇒
//      CardParser
//        .parseTrick(s)
//        .fold(retryReadTrick)(t ⇒ IO(t))
//    )
//
//  def retryReadHand: IO[Hand] = // todo real implementation
//    IO.readLn.flatMap(s ⇒ CardParser.parseHand(s).fold(retryReadHand)(t ⇒ IO(t)))
//
////  override def apply[A](fa: Action[A]): IO[A] = fa match {
////    case ObserveTrick ⇒ retryReadTrick
////    case PlayCard(c) ⇒ IO.putStrLn(s"Play $c")
////    case LookHand ⇒ retryReadHand
////    case FinishTrick(partial) ⇒
////  }
//}

object Test extends App {
  import ActionF._

  def updatedPartnerships(picker: Picker, pc: PartnerCard, me: Seat)
                         (currentTrick: CurrentTrick)
                         (partnership: Map[Seat, Partnership]): Map[Seat, Partnership] = {

    import Picker.WeHaveAPicker, Partnership.{Partner, Opponent}

    (picker, currentTrick) match {
      case (WeHaveAPicker(pickerSeat), PreviousPlays(trick)) ⇒
        implicit val o = trick.cardOrder
        filterFF(trick.plays.toList)(_._2 === pc.c)(listInstance: Foldable[List], net.arya.util.Pointed.applicativePointed[List], listInstance: PlusEmpty[List]).map(_._1).headOption.fold(partnership) { pcardPlayer ⇒
          partnership.map {
            case (s, _) ⇒
              (s,  if ((pickerSeat === me) === (pcardPlayer === s)) Partner else Opponent)
          }
        }
      case _ ⇒ partnership
    }

  }

  def program(brain: Brain): FreeC[Action,Unit] = for {

    currentTrick  ← observeTrick
    brainv2       = brain.updatedPartnerships(currentTrick)
    cardChoice    = brainv2.unsafeRandomLegal(currentTrick) // todo
    _             ← playCard(cardChoice)
    completeTrick ← finishTrick(currentTrick.add(brain.mySeat, cardChoice))

    // <magic>

    brainv3             ← (brain.hand delete cardChoice) match {
      case EmptySet(_) ⇒ scoring // end of hand
      case NonEmptySet(x, ys) ⇒
        val remainingHand = NonEmptySet(x, ys)
        program(
          Brain.hand.set(remainingHand)(
            Brain.pastTricks.modify(completeTrick.t :: _)(brainv2)
          )
        )
    }

  } yield ()
}

