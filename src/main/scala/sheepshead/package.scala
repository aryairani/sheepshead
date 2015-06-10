import net.arya.util.NonEmptySet

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

  // general utilities
  def atRandom[F[_]:Foldable,A](elems: F[A]): Option[A] = {
    val index = (scala.math.random * elems.count).toInt
    elems.index(index)
  }

  def atRandom1[F[_]:Foldable1,A](elems: F[A]): A = atRandom(elems).get

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1Nel[F[_]:Foldable1,A](input: F[A])(test: A ⇒ Boolean): NonEmptyList[A] =
    filter1FG[F,NonEmptyList,A](input)(test)

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1FF[F[_]:Foldable1:Pointed:Plus,A](input: F[A])(test: A ⇒ Boolean): F[A] =
    filter1FG[F,F,A](input)(test)

  /** Provided that `test` is guaranteed to return true for at least one element,
    * return a nonzero subset of elements passing the test */
  def filter1FG[F[_]:Foldable1,G[_]:Foldable1:Pointed:Plus,A]
  (input: F[A])(test: A ⇒ Boolean): G[A] =
    input.foldMap1(_.point[G])(Semigroup.instance[G[A]](
      (g1, g2) ⇒
        (g1.all(test), g2.all(test)) match {
          case (true, true) ⇒ g1 <+> g2
          case (true, false) ⇒ g1
          case (false, true) ⇒ g2
          case _ ⇒ g1 // either; it should get thrown away
        }
    )) <| (result ⇒ assert(result.all(test), "test returned false for all inputs"))

  def filterFG[F[_]:Foldable,G[_]:Pointed:PlusEmpty,A]
  (input: F[A])(test: A ⇒ Boolean): G[A] =
    input.foldMap[G[A]]( a ⇒ test(a).fold[G[A]](a.point[G], PlusEmpty[G].empty) )(PlusEmpty[G].monoid)

  def filterFF[F[_]:Foldable:Pointed:PlusEmpty,A]
  (input: F[A])(test: A ⇒ Boolean): F[A] =
    input.foldMap[F[A]]( a ⇒ test(a).fold[F[A]](a.point[F], PlusEmpty[F].empty) )(PlusEmpty[F].monoid)

  def unsafeListToNel[A]: List[A] ⇒ NonEmptyList[A] = {
    case head :: tail ⇒ NonEmptyList(head, tail: _*)
    case _ ⇒ throw new IllegalArgumentException("input can't be empty")
  }

  def unsafeListToNes[A]: List[A] ⇒ NonEmptySet[A] = {
    case head :: tail ⇒ NonEmptySet(head, tail.toSet)
    case _ ⇒ throw new IllegalArgumentException("input can't be empty")
  }
}
