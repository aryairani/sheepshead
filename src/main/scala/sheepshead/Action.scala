package sheepshead

import sheepshead.brain.{PreviousPlays, CurrentTrick}

import scalaz.Free.liftFC

sealed trait Action[A] extends Product with Serializable
case class PlayCard(c: Card)                   extends Action[Unit]
case object LookHand                           extends Action[Hand]
case object ObserveTrick                       extends Action[CurrentTrick]
case class FinishTrick(partial: PreviousPlays) extends Action[PreviousPlays]
case object Scoring                            extends Action[Unit]

object ActionF {
  def playCard(c: Card)                   = liftFC[Action, Unit]          (PlayCard(c))
  def observeTrick                        = liftFC[Action, CurrentTrick]  (ObserveTrick)
  def finishTrick(partial: PreviousPlays) = liftFC[Action, PreviousPlays] (FinishTrick(partial))
  def scoring                             = liftFC[Action, Unit]          (Scoring)
}
