package sheepshead

import net.arya.util.{EmptySet, NonEmptySet}
import net.arya.util.filter._
import sheepshead.brain._
import sheepshead.util.CardParser

import scalaz._, Free.FreeC, scalaz.effect.{IORef, IO}
import scalaz.syntax.monad._

class ConsoleAction(seat: Seat) extends (Action ~> IO)
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

  override def apply[A](fa: Action[A]): IO[A] = fa match {
    case ObserveTrick         ⇒ retryReadTrick(seat)
    case PlayCard(c)          ⇒ IO.putStrLn(s"Play $c")
    case LookHand             ⇒ retryReadHand
    case FinishTrick(partial) ⇒ retryFinishReadTrick(partial, seat)
  }
}

object Test extends App {

  import ActionF._
  import net.arya.util.freec._

  type FAction[A] = FreeC[Action, A]

  def playOneTrick(brain: Brain): FAction[Map[Set[Seat], Int]] = {
    for {

      firstHalf ← observeFirstHalf
      brainv2 = brain.updatedPartnerships(firstHalf)

      cardChoice = brainv2.unsafeRandomLegal(firstHalf) // todo

      _ ← playCard(cardChoice)
      firstHalfPlusMe = firstHalf.add(brain.mySeat, cardChoice)
      completeTrick ← if (firstHalfPlusMe.t.playCards.size == 5)
        firstHalfPlusMe.point[FAction]
      else
        finishTrick(firstHalfPlusMe)

      brainv3 = Brain.pastTricks.modify(completeTrick.t :: _)(brainv2)
      remainingHand = (brain.hand delete cardChoice)
      score ← remainingHand match {
        case NonEmptySet(x, ys) ⇒
          val h = NonEmptySet(x, ys)
          def brainv4 = Brain.hand.set(h)(brainv3)
          playOneTrick(brainv4)
        case EmptySet(_) ⇒
          val teams: Set[Set[Seat]] = Set()
          trickScores(teams, brainv3.pastTricks).point[FAction]
      }
//      _ ← cleanup
    } yield score
  }

}

class StateAlgebra[S] {
  sealed trait StateAction[A]
  case class ConstantState[A](a: A, s: () ⇒ S) extends StateAction[A]
  case class State[A](a: A)                    extends StateAction[A]
  case object Get                              extends StateAction[S]
  case class Gets[T](f: S ⇒ T)                 extends StateAction[T]
  case class Put[T](s: S)                      extends StateAction[Unit]
  case class Modify(f: S ⇒ S)                  extends StateAction[Unit]

  object StateAction {
    def stateIO(ref: IORef[S]) = new (StateAction ~> IO) {
      override def apply[A](fa: StateAction[A]): IO[A] = fa match {
        case ConstantState(a, s) ⇒ ref.write(s()).map(_ ⇒ a)
        case State(a) ⇒ IO(a)
        case Get ⇒ ref.read
        case Gets(f) ⇒ ref.read.map(f)
        case Put(s) ⇒ ref.write(s)
        case Modify(f) ⇒ ref.mod(f).void
      }
    }

    implicit val stateState = new (StateAction ~> scalaz.State[S, ?]) {
      override def apply[A](fa: StateAction[A]): scalaz.State[S, A] = fa match {
        case ConstantState(a, s) ⇒ scalaz.State.constantState(a, s())
        case State(a) ⇒ scalaz.State.state(a)
        case Get ⇒ scalaz.State.get
        case Gets(f) ⇒ scalaz.State.gets(f)
        case Put(s) ⇒ scalaz.State.put(s)
        case Modify(f) ⇒ scalaz.State.modify(f)
      }
    }

    implicit def stateStateT[F[_] : Applicative] = new (StateAction ~> scalaz.StateT[F, S, ?]) {
      override def apply[A](fa: StateAction[A]): scalaz.StateT[F, S, A] = fa match {
        case ConstantState(a, s) ⇒ scalaz.StateT.constantStateT(a)(s())
        case State(a) ⇒ scalaz.StateT.stateT(a)
        case Get ⇒ scalaz.StateT[F, S, A](s ⇒ (s, s).point[F])
        case Gets(f) ⇒ scalaz.StateT[F, S, A](s ⇒ (s, f(s)).point[F])
        case Put(s) ⇒ scalaz.StateT[F, S, A](_ ⇒ (s, ()).point[F])
        case Modify(f) ⇒ scalaz.StateT[F, S, A](s ⇒ (f(s), ()).point[F])
      }
    }
  }
}
