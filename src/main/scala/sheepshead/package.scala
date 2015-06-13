import net.arya.util.NonEmptySet
import net.arya.util.unsafe.{unsafeListToNes, unsafeListToNel}
import net.arya.util.filter._

import scalaz._, Scalaz._, Ordering._

package object sheepshead {
  import net.arya.util.Pointed, Pointed.pointedSyntax

  type Hand = NonEmptySet[Card]
  type Blind = (Card,Card)

  val fulldeck: Set[Card] = for {
    rank ← Rank.all
    suit ← Suit.all
  } yield Card(rank, suit)

  def deal5: (Hand,Hand,Hand,Hand,Hand,Blind) =
    scala.util.Random.shuffle(fulldeck.toVector).toList
      .grouped(6).toList match {
      case h1 :: h2 :: h3 :: h4 :: h5 :: List(k1,k2) :: Nil ⇒
        (h1 |> unsafeListToNes,
          h2 |> unsafeListToNes,
          h3 |> unsafeListToNes,
          h4 |> unsafeListToNes,
          h5 |> unsafeListToNes,
          (k1,k2))
    }

  def wouldLead(card: Card)(trick: Trick): Boolean = {
    import trick.cardOrder
    card > trick.playCards.maximum1
  }

  def wouldLead[F[_]:Foldable:Pointed:PlusEmpty](hand: F[Card])(trick: Trick): F[Card] = {
    import trick.cardOrder
    filterFF(hand)(_ > trick.playCards.maximum1)
  }

  def filterLegal[F[_]:Foldable1:Pointed:Plus](hand: F[Card])(led: SSuit): F[Card] =
    filter1FF(hand)(canFollow(led)(_, hand))

  implicit class Foldable1ExtraOps[F[_]:Foldable1,A](fa: F[A]) {
    def toNel: NonEmptyList[A] = fa.toList |> unsafeListToNel
  }

  def filterLegalNel[F[_]:Foldable1](hand: F[Card])(led: SSuit): NonEmptyList[Card] =
    filter1Nel(hand)(canFollow(led)(_, hand))


  def canFollow[F[_]:Foldable](led: SSuit)(card: Card, hand: F[Card]): Boolean =
    hand.any(led.members.contains) --> led.members.contains(card)

  def trickScores(teams: Set[Set[Seat]], tricks: List[Trick]): Map[Set[Seat],Int] = {
    def oneSeatOneTrick(seat: Seat, trick: Trick) = trick.playMap.get(seat).map(_.points).orZero
    def oneTeamOneTrick(team: Set[Seat], trick: Trick) = team.map(oneSeatOneTrick(_, trick)).suml
    outlaws.std.set.setOutlawInstnace.fproduct(teams)(team ⇒ tricks.map(oneTeamOneTrick(team, _)).suml).toMap
  }


}
