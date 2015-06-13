package sheepshead.effect

import scalaz._
import scalaz.Free.FreeC

object Coproduct {

  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): FreeC[G, A] =
    Free.liftFC(I.inj(fa))

  def or[F[_], G[_], H[_]](f: F ~> H, g: G ~> H): ({type cp[α]=Coproduct[F,G,α]})#cp ~> H = new NaturalTransformation[({type cp[α]=Coproduct[F,G,α]})#cp,H] {
    def apply[A](fa: Coproduct[F,G,A]): H[A] = fa.run match {
      case -\/(ff) ⇒ f(ff)
      case \/-(gg) ⇒ g(gg)
    }
  }
}

