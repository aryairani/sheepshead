package sheepshead

import net.arya.util.NonEmptySet
import sheepshead.brain._
import sheepshead.util.CardParser

import scalaz.Free.FreeC
import scalaz.effect.IO
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.std.option._
import scalaz.{PlusEmpty, Foldable, ~>}
import scalaz.syntax.foldable._
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalaz.std.list._

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

object EmptySet0 {
  def unapply[A](s: Set[A]): Option[Unit] = s.isEmpty.option(())
}
object NonEmptySet0 {
  def unapply[A](s: Set[A]): Option[(A,Set[A])] = s.isEmpty.fold(none, (s.head, s.tail).some)
}

object Test extends App {
  import ActionF._

  def updatedPartnerships(picker: Picker, pc: PartnerCard, me: Seat)
                         (currentTrick: CurrentTrick)
                         (partnership: Map[Seat, Partnership]): Map[Seat, Partnership] = {

    import Picker.{Yes ⇒ Picker}, Partnership.{Partner, Opponent}

    (picker, currentTrick) match {
      case (Picker(pickerSeat), PreviousPlays(trick)) ⇒
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
    completeTrick ← finishTrick(currentTrick.add(brain.seat, cardChoice))

    // <magic>

    brainv3             ← (brain.hand delete cardChoice) match {
      case EmptySet0(_) ⇒ scoring // end of hand
      case NonEmptySet0(x, ys) ⇒
        val remainingHand = NonEmptySet(x, ys)
        program(
          Brain.hand.set(remainingHand)(
            Brain.pastTricks.modify(completeTrick.t :: _)(brainv2)
          )
        )
    }

  } yield ()
}

