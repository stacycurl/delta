package sjc.delta.cats

import scala.language.higherKinds

import sjc.delta.Delta
import sjc.delta.Delta.Aux

import cats.functor.{Contravariant, Profunctor}
import cats.{Applicative, Monad}



object instances {
  implicit def deltaApplicative[In]: Applicative[Aux[In, ?]] = new Applicative[Aux[In, ?]] {
    def map[A, B](deltaA: Aux[In, A])(ab: A ⇒ B):              Aux[In, B] = deltaA.map(ab)
    def ap[A, B](deltaAB: Aux[In, A ⇒ B])(deltaA: Aux[In, A]): Aux[In, B] = deltaA.applyTo(deltaAB)
    def product[A, B](deltaA: Aux[In, A], deltaB: Aux[In, B]): Aux[In, (A, B)] = deltaA.zip(deltaB)
    def pure[A](a: A):                                         Aux[In, A] = Delta.const[In, A](a)
  }

  implicit def deltaMonad[In]: Monad[Aux[In, ?]] = new Monad[Aux[In, ?]] {
    def pure[A](a: A):                                                Aux[In, A] = Delta.const[In, A](a)
    def flatMap[A, B](deltaA: Aux[In, A])(aDeltaB: (A) ⇒ Aux[In, B]): Aux[In, B] = deltaA.flatMap(aDeltaB)
  }

  implicit def deltaContravariant[Out]: Contravariant[Aux[?, Out]] = new Contravariant[Aux[?, Out]] {
    def contramap[A, B](delta: Aux[A, Out])(f: B ⇒ A): Aux[B, Out] = delta.contramap(f)
  }

  implicit val deltaProfunctor: Profunctor[Aux] = new Profunctor[Aux] {
    def dimap[A, B, C, D](delta: Aux[A, B])(f: C ⇒ A)(g: B ⇒ D): Aux[C, D] = delta.dimap(f, g)
  }
}