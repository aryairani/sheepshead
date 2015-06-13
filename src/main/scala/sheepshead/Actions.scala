package sheepshead

import net.arya.util.{EmptySet, NonEmptySet}
import net.arya.util.filter._
import sheepshead.brain._
import sheepshead.util.CardParser

import scalaz._, Free.FreeC, effect.IO
import scalaz.syntax.bind._
import scalaz.syntax.applicative._

object ConsoleAction
//  extends (Action ~> IO)
{

  def retryReadTrick(seat: Seat): IO[CurrentTrick] =
    IO.putStrLn("Ok, what's been played?") >>
      IO.readLn.flatMap(str ⇒
        CardParser
          .parseTrick(seat)(str)
          .fold(
            IO.putStrLn("Couldn't parse that, try again?") >>
              retryReadTrick(seat)
          )(t ⇒ IO(t))
      )

  def retryFinishReadTrick(previousPlays: PreviousPlays, seat: Seat): IO[CurrentTrick] =
    IO.putStrLn(s"What was played after ${previousPlays.t.playCards.list.mkString(", ")}?") >>
      IO.readLn.flatMap(str ⇒
        CardParser
          .parseTrick(seat)(str)
          .fold(
            IO.putStrLn("Couldn't parse that, try again?") >>
              retryReadTrick(seat)
          )(t ⇒ IO(t))
      )

  def retryReadHand: IO[Hand] = // todo real implementation
    IO.putStrLn("What's in my hand?") >>
      IO.readLn.flatMap(s ⇒ CardParser.parseHand(s).fold(
        IO.putStrLn("Couldn't parse that, try again?") >>
          retryReadHand
      )(t ⇒ IO(t)))

//  override def apply[A](fa: Action[A]): IO[A] = fa match {
//    case ObserveTrick ⇒ retryReadTrick
//    case PlayCard(c) ⇒ IO.putStrLn(s"Play $c")
//    case LookHand ⇒ retryReadHand
//    case FinishTrick(partial) ⇒ retryFinishReadTrick(partial, seat)
//  }
}

object Test extends App {
  import ActionF._
  import net.arya.util.freec._

  def playOneTrick(brain: Brain): FreeC[Action,Map[Seat,Int]] = for {

    firstHalf       ← observeFirstHalf
    brainv2         = brain.updatedPartnerships(firstHalf)

    cardChoice      = brainv2.unsafeRandomLegal(firstHalf) // todo

    _               ← playCard(cardChoice)
    firstHalfPlusMe = firstHalf.add(brain.mySeat, cardChoice)
    completeTrick   ← if (firstHalfPlusMe.t.playCards.size == 5)
                        firstHalfPlusMe.point[FreeC[Action,?]]
                      else
                        finishTrick(firstHalfPlusMe)

    remainingHand   = (brain.hand delete cardChoice)
    score           ← remainingHand match {
      case NonEmptySet(x, ys) ⇒
        val h = NonEmptySet(x,ys)
        def brainv3 = Brain.hand.set(h)(brainv2)
        def brainv4 = Brain.pastTricks.modify(completeTrick.t :: _)(brainv3)
        playOneTrick(brainv4)
      case EmptySet(_) ⇒ scoring // end of hand
    }
  } yield score
}

