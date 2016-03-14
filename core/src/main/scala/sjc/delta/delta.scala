package sjc.delta

import scala.language.implicitConversions


trait Delta[In] {
  type Out

  def apply(left: In, right: In): Out
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  def from[In] = new From[In]

  class From[In] {
    def curried[Out](f: In ⇒ In ⇒ Out): Aux[In, Out] = apply(Function.uncurried(f))
    def apply[Out](f: (In, In) ⇒ Out): Aux[In, Out] = new Function[In, Out](f)
  }

  implicit class DeltaDeltaOps[A, B](val delta: Aux[A, B]) extends AnyVal {
    def map[C](f: B ⇒ C):         Delta.Aux[A, C]  = new Delta.Mapped[A, B, C](delta, f)
    def contramap[In](f: In ⇒ A): Delta.Aux[In, B] = new Delta.Contramapped[A, B, In](delta, f)
    def dimap[In, C](f: In ⇒ A, g: B ⇒ C): Delta.Aux[In, C] = new Delta.DiMapped[In, A, B, C](delta, f, g)

    def andThen[C](out: B)(implicit deltaOut2: Delta.Aux[B, C]): Delta.Aux[A, C] =
      new Delta.AndThen[A, B, C](out, delta, deltaOut2)

    def lift[In]: Aux[In ⇒ A, In ⇒ B] = function.function1Delta[In, A, B](delta)
  }

  implicit class DeltaOps[In](val left: In) extends AnyVal {
    def delta(right: In)(implicit delta: Delta[In]): delta.Out = delta(left, right)
  }

  object function {
    implicit def function1Delta[A, B, C](implicit delta: Aux[B, C]): Aux[A ⇒ B, A ⇒ C] = new Delta[A ⇒ B] {
      type Out = A ⇒ C

      def apply(left: A ⇒ B, right: A ⇒ B): Out = (a: A) ⇒ delta(left(a), right(a))
    }
  }

  object fallback {
    implicit def fallbackDelta[A]: Aux[A, (A, A)] = new Delta[A] {
      type Out = (A, A)

      def apply(left: A, right: A): (A, A) = (left, right)
    }
  }

  private class Function[In, Out0](f: (In, In) ⇒ Out0) extends Delta[In] {
    type Out = Out0

    def apply(left: In, right: In): Out = f(left, right)
  }

  private class Mapped[A, B, C](delta: Aux[A, B], f: B ⇒ C) extends Delta[A] {
    type Out = C

    def apply(left: A, right: A): Out = f(delta(left, right))
  }

  private class Contramapped[A, B, C](delta: Aux[A, B], f: C ⇒ A) extends Delta[C] {
    type Out = B

    def apply(left: C, right: C): Out = delta(f(left), f(right))
  }

  private class DiMapped[A, B, C, D](delta: Aux[B, C], f: A ⇒ B, g: C ⇒ D) extends Delta[A] {
    type Out = D

    def apply(left: A, right: A): D = g(delta(f(left), f(right)))
  }

  private class AndThen[In, Out1, Out2](
    out1: Out1, first: Aux[In, Out1], second: Aux[Out1, Out2]
  ) extends Delta[In] {

    type Out = Out2

    def apply(left: In, right: In): Out2 = {
      val firstOrder  = first.apply(left, right)
      val secondOrder = second.apply(firstOrder, out1)

      secondOrder
    }
  }
}

trait Patch[P] {
  def nonEmpty(patch: P): Boolean = !isEmpty(patch)
  def isEmpty(patch: P): Boolean
}

object Patch {
  def apply[P](implicit empty: Patch[P]): Patch[P] = empty

  def create[P](isEmptyFn: P ⇒ Boolean): Patch[P] = new Patch[P] {
    def isEmpty(patch: P): Boolean = isEmptyFn(patch)
  }
}