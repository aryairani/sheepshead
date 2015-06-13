package sheepshead
package effect.egocentric

import sheepshead.brain.{CurrentTrick, PreviousPlays}

//trait Card; trait Hand; trait CurrentTrick; trait PreviousPlays

import scalaz.Free.{FreeC, liftFC}
import scalaz.Inject

sealed trait Action[A] extends Product with Serializable
case class  PlayCard(c: Card)                         extends Action[Unit]
case object LookHand                                  extends Action[Hand]
case object ObserveFirstHalf                          extends Action[CurrentTrick]
case class  ObserveSecondHalf(partial: PreviousPlays) extends Action[PreviousPlays]

class ActionLang[F[_]](implicit I: Inject[Action,F]) {

  import sheepshead.effect.Coproduct.{lift,or}

  def playCard(c: Card)                     : FreeC[F,Unit]          = lift(PlayCard(c))
  val observe1stHalf                        : FreeC[F,CurrentTrick]  = lift(ObserveFirstHalf)
  def observe2ndHalf(partial: PreviousPlays): FreeC[F,PreviousPlays] = lift(ObserveSecondHalf(partial))
  val lookHand                              : FreeC[F,Hand]          = lift(LookHand)
}
object ActionLang {
  implicit def actionLang[F[_]](implicit I: Inject[Action,F]): ActionLang[F] = new ActionLang[F]
}