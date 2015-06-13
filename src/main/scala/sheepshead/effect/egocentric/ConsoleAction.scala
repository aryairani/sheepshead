package sheepshead.effect.egocentric

import sheepshead._
import sheepshead.brain.{CurrentTrick, PreviousPlays}
import sheepshead.util.CardParser

import scalaz._
import scalaz.effect.IO
import scalaz.syntax.monad._

class ConsoleAction(seat: Seat) extends (Action ~> IO) {

  override def apply[A](fa: Action[A]): IO[A] = fa match {
    case ObserveFirstHalf         ⇒ retryReadTrick(seat)
    case PlayCard(c)          ⇒ IO.putStrLn(s"Play $c")
    case LookHand             ⇒ retryReadHand
    case ObserveSecondHalf(partial) ⇒ retryFinishReadTrick(partial, seat)
  }

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

  def retryFinishReadTrick(previousPlays: PreviousPlays, seat: Seat): IO[PreviousPlays] = {
    if (previousPlays.t.playCards.size < 5)
      IO.putStrLn(s"What was played after ${previousPlays.t.playCards.list.mkString(", ")}?") >>
        IO.readLn.flatMap(str ⇒
          CardParser
            .parseTrickMandatory(seat)(str)
            .fold(
              IO.putStrLn("Couldn't parse that, try again?") >>
                retryFinishReadTrick(previousPlays, seat)
            )(t ⇒ IO(t))
        )
    else IO(previousPlays)
  }

  def retryReadHand: IO[Hand] = // todo real implementation
    IO.putStrLn("What's in my hand?") >>
      IO.readLn.flatMap(s ⇒ CardParser.parseHand(s).fold(
        IO.putStrLn("Couldn't parse that, try again?") >>
          retryReadHand
      )(t ⇒ IO(t)))
}
