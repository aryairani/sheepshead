package sheepshead

import scalaz._, Scalaz._
import Rank._, Suit._, SSuit._

case class Card(rank: Rank, suit: Suit) {
  override def toString = s"$rank$suit"
  def isTrump = suit === ♦ || rank === Q || rank === J
  def points = rank.points
  def ssuit: SSuit = if (isTrump) SSuit.T_♦ else suitToSSuit(suit)
}

object Card {
  val nontrumpRankOrder = Order[Int].reverseOrder.contramap[Rank](
    List[Rank](A, _10, K, Q, J, _9, _8, _7).indexOf
  )
  val trumpRankOrder = Order[Int].reverseOrder.contramap[Rank](
    List[Rank](Q, J, A, _10, K, _9, _8, _7).indexOf
  )
  val trumpSuitOrder = Order[Int].reverseOrder.contramap[Suit](
    List[Suit](♣, ♠, ♥, ♦).indexOf
  )

  def cardOrder(led: SSuit): Order[Card] = Order.order {
    case (x, y) ⇒
      (x.ssuit, y.ssuit) match {
        case (T_♦, T_♦) ⇒ // both are trump
          trumpRankOrder.contramap[Card](_.rank) |+|
            trumpSuitOrder.contramap[Card](_.suit) order(x,y)

        case (T_♦, _) ⇒ Ordering.GT
        case (_, T_♦) ⇒ Ordering.LT
        case (fail1, fail2) ⇒ // neither are trump

          if(fail1 === led && fail2 === led) // both in led suit
            nontrumpRankOrder.contramap[Card](_.rank).order(x,y)

          else // whichever matches the led suit
            Order[Boolean].contramap[Card](_.ssuit === led).order(x,y)

        case _ ⇒ // whichever is trump
          Order[Boolean].contramap[Card](_.isTrump).order(x,y)
      }
  }

}
