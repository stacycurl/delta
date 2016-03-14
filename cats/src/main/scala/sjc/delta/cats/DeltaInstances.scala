package sjc.delta.cats

import cats.Functor
import cats.functor.{Profunctor, Contravariant}
import sjc.delta.Delta
import sjc.delta.Delta.Aux


object DeltaInstances {
  def deltaFunctor[In]: Functor[Delta.Aux[In, ?]] = new Functor[Delta.Aux[In, ?]] {
    def map[A, B](delta: Aux[In, A])(f: A ⇒ B): Aux[In, B] = delta.map(f)
  }

  def deltaContravariant[Out]: Contravariant[Delta.Aux[?, Out]] = new Contravariant[Delta.Aux[?, Out]] {
    def contramap[A, B](delta: Aux[A, Out])(f: B ⇒ A): Aux[B, Out] = delta.contramap(f)
  }

  val deltaProfunctor: Profunctor[Delta.Aux] = new Profunctor[Aux] {
    def dimap[A, B, C, D](delta: Aux[A, B])(f: C ⇒ A)(g: B ⇒ D): Aux[C, D] = delta.dimap(f, g)
  }
}
