package sheepshead.effect.egocentric

import net.arya.util.{EmptySet, NonEmptySet}
import net.arya.util.freec._
import sheepshead._
import sheepshead.brain._
import sheepshead.effect.state.{StateAlgebra, StateLang}

import scalaz.Free.FreeC
import scalaz._

object Program extends App {

  type Sheep[A] = Coproduct[Action, StateAlgebra[Brain,?], A]
  type SheepC[A] = FreeC[Sheep,A]
  implicit class P[A](a: A) {
    def point: FreeC[Sheep,A] = Monad[SheepC].point(a)
  }

  def groupTeams(picker: Picker, partnership: Map[Seat, Partnership]) =
    picker.fold[Set[Set[Seat]]](
      leaster = partnership.keySet.map(Set(_)), // every man for himself
      (_,_) ⇒ partnership
        .toSet[(Seat,Partnership)]
        .groupBy(_._2)
        .mapValues(_.map(_._1))
        .values
        .toSet
    )

  def playOneTrick(implicit
                   A: ActionLang[Sheep],
                   B: StateLang[Brain,Sheep]): SheepC[Map[Set[Seat],Int]] = {

    import A._, Brain.{pastTricks}

    def updateBrain(f: Brain ⇒ Brain) = B.modify(f)

    for {

      firstHalf ← observe1stHalf
      b ← updateBrain(_.updatedPartnerships(firstHalf))

      cardChoice = b.unsafeRandomLegal(firstHalf) // todo

      _ ← playCard(cardChoice)

      secondHalf ← {
        val withMe = firstHalf.add(b.mySeat, cardChoice)

        if (withMe.size == 5)
          withMe.point
        else
          observe2ndHalf(withMe)
      }

      b ← updateBrain(pastTricks.modify(secondHalf.t :: _))

      score ← (b.hand delete cardChoice) match {
        case NonEmptySet(h) ⇒
          updateBrain(Brain.hand.set(h)).flatMap(_ ⇒ playOneTrick)
        case EmptySet() ⇒
          trickScores(groupTeams(b.picker, b.partnership), b.pastTricks).point
      }
    } yield score
  }
}

