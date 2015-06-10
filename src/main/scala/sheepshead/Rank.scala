package sheepshead

import scalaz.Equal

sealed trait Rank extends Product with Serializable {
  def apply(s: Suit) = Card(this, s)
  def points = Rank.points(this)

  import Rank._
  override def toString = this match {
    case `_10` ⇒ "10"
    case `_9` ⇒ "9"
    case `_8` ⇒ "8"
    case `_7` ⇒ "7"
    case A ⇒ "A"
    case K ⇒ "K"
    case Q ⇒ "Q"
    case J ⇒ "J"
  }
}
object Rank {
  case object A extends Rank
  case object K extends Rank
  case object Q extends Rank
  case object J extends Rank
  case object _10 extends Rank
  case object _9 extends Rank
  case object _8 extends Rank
  case object _7 extends Rank

  val all: Set[Rank] = Set(A, _10, K, Q, J, _9, _8, _7)

  val points: Rank => Int = Map[Rank,Int](
    A → 11,  _10 → 10,  K → 4,  Q → 3,  J → 2
  ).withDefaultValue(0)

  implicit val rankEqual: Equal[Rank] = Equal.equalRef
}






