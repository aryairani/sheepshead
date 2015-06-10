package sheepshead
package brain

import scalaz.Scalaz._

sealed trait CurrentTrick {
  def fold[B](yourLead: ⇒ B, previousPlays: Trick ⇒ B): B = this match {
    case YourLead ⇒ yourLead
    case PreviousPlays(t) ⇒ previousPlays(t)
  }

  def toOption: Option[Trick] = fold(None, Some.apply)

  def add(s: Seat, c: Card): PreviousPlays =
    PreviousPlays(fold(Trick.lead(s,c), _.add(s, c)))

  lazy val points: Int = fold(0, _.playCards.foldMap(_.points))
}

case object YourLead extends CurrentTrick
final case class PreviousPlays(t: Trick) extends CurrentTrick
