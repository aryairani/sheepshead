package sheepshead

import org.specs2._
import scalaz._, Scalaz._

import Rank._, Suit._, SSuit._, brain._

import scalaz.{Semigroup, NonEmptyList}

class TrickTest extends mutable.Specification with ScalaCheck {
  val buildPhonyPlay: NonEmptyList[Rank] => PreviousPlays =
    set => PreviousPlays(Trick(F_♣, Seat.seatNEL(Seat.S1) zip set.map(_.apply(spade))))


  "empty trick worth 0 points" >> {
    YourLead.points shouldEqual 0
  }

  val examples = NonEmptyList(
    NonEmptyList(A, K, _9, _8) -> 15,
    NonEmptyList(J, _7) -> 2
  )

  "point counting examples" >> {
    examples.foldMap1 {
      case (ranks,points) ⇒

        s"$ranks worth $points points" >> {
          buildPhonyPlay(ranks).points shouldEqual points
        }

    }(Semigroup.lastSemigroup)
  }
}
