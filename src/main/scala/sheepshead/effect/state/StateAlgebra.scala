package sheepshead.effect.state


sealed trait StateAlgebra[S,A]
case class Point[S,A](a: A)                    extends StateAlgebra[S,A]
case class Get[S]()                            extends StateAlgebra[S,S]
case class Put[S](s: S)                        extends StateAlgebra[S,Unit]

import scalaz.Inject

class StateLang[S,F[_]](implicit I: Inject[StateAlgebra[S,?],F]) {
  import sheepshead.effect.Coproduct.lift
  type L[a] = StateAlgebra[S,a]

  def point[A](a: A) = lift[L,F,A](Point(a))
  def get            = lift[L,F,S](Get())
  def put(s: S)      = lift[L,F,Unit](Put(s))
  def modify(f: S ⇒ S) = get.flatMap(s ⇒ put(f(s))).flatMap(_ ⇒ get)
}

object StateLang {
  implicit def stateLang[S,F[_]](implicit I: Inject[StateAlgebra[S,?],F]) = new StateLang[S,F]
}




object Interpreter {
  import scalaz.{Applicative, ~>}
  import scalaz.effect.{IO, IORef}
  import scalaz.syntax.monad._

  def stateIO[S](ref: IORef[S]) = new (StateAlgebra[S,?] ~> IO) {
    override def apply[A](fa: StateAlgebra[S,A]): IO[A] = fa match {
      case Point(a) ⇒ IO(a)
//      case Get() ⇒ ref.read
      case _ : Get[S] ⇒ ref.read
      case Put(s) ⇒ ref.write(s)
    }
  }

  implicit def stateState[S] = new (StateAlgebra[S,?] ~> scalaz.State[S, ?]) {
    override def apply[A](fa: StateAlgebra[S,A]): scalaz.State[S, A] = fa match {
      case Point(a) ⇒ scalaz.State.state(a)
//      case Get() ⇒ scalaz.State.get
      case _ : Get[S] ⇒ scalaz.State.get
      case Put(s) ⇒ scalaz.State.put(s)
    }
  }

  implicit def stateStateT[F[_] : Applicative, S] = new (StateAlgebra[S,?] ~> scalaz.StateT[F, S, ?]) {
    override def apply[A](fa: StateAlgebra[S,A]): scalaz.StateT[F, S, A] = fa match {
      case Point(a) ⇒ scalaz.StateT.stateT(a)
//      case Get() ⇒ scalaz.StateT[F, S, A](s ⇒ (s, s).point[F])
      case _ : Get[S] ⇒ scalaz.StateT[F, S, A](s ⇒ (s, s).point[F])
      case Put(s) ⇒ scalaz.StateT[F, S, A](_ ⇒ (s, ()).point[F])
    }
  }
}
