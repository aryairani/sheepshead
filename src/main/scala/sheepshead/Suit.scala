package sheepshead

import sheepshead.Suit.{♥, ♠, ♣}

import scalaz.Equal

sealed trait Suit extends Product with Serializable
object Suit {
  case object ♣ extends Suit
  case object ♠ extends Suit
  case object ♥ extends Suit
  case object ♦ extends Suit
  val club = ♣
  val spade = ♠
  val heart = ♥
  val diamond = ♦

  val all: Set[Suit] = Set(♣, ♠, ♥, ♦)

  implicit val suitEqual: Equal[Suit] = Equal.equalRef
}

sealed class SSuit(val members: Set[Card])
object SSuit {
  import Rank._
  val fail: Suit ⇒ Set[Card] = s ⇒ Set[Rank](A, _10, K, _9, _8, _7).map(_(s))
  case object F_♣ extends SSuit(fail(Suit.♣))
  case object F_♠ extends SSuit(fail(Suit.♠))
  case object F_♥ extends SSuit(fail(Suit.♥))
  case object T_♦ extends SSuit(Suit.all.map(Q.apply) ++ Suit.all.map(J.apply) ++ fail(Suit.♦))

  val all: Set[SSuit] = Set(F_♣, F_♠, F_♥, T_♦)

  val suitToSSuit: Map[Suit,SSuit] = Map(♣ → F_♣, ♠ → F_♠, ♥ → F_♥)
  val ssuitToSuit: Map[SSuit,Suit] = suitToSSuit.map(_.swap)

  implicit val ssuitEqual: Equal[SSuit] = Equal.equalRef
}
