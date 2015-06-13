package sheepshead

import scalaz._, Scalaz._

case class Trick(ssuit: SSuit, plays: NonEmptyList[(Seat,Card)]) {
  def add(s: Seat, c: Card) = Trick(ssuit, (s,c) <:: plays)
  def playCards: NonEmptyList[Card] = plays.map(_._2)
  lazy val playMap: Map[Seat,Card] = plays.list.toMap

  def points: Int = playCards.foldMap1(_.points)

  def leading: (Seat, Card) = plays.maximumBy1(_._1)
  def leadingSeat: Seat = leading._1
  def leadingCard: Card = leading._2

  implicit val cardOrder = Card.cardOrder(ssuit)
  implicit val seatOrder: Enum[Seat] = Seat.playSequenceEnum(plays.last._1)
}
object Trick {
  def lead(s: Seat, c: Card) = Trick(c.ssuit, (s,c).wrapNel)
}