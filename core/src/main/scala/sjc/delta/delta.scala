package sjc.delta

import scala.language.implicitConversions


trait Delta[In] {
  type Out

  def apply(left: In, right: In): Out

  def map[B](f: Out => B): Delta.Aux[In, B] = new Delta.Mapped[In, Out, B](f, this)
  def contramap[B](f: B => In): Delta.Aux[B, Out] = new Delta.Contramapped[In, Out, B](f, this)

  def andThen[Out2](out: Out)(implicit deltaOut2: Delta.Aux[Out, Out2]): Delta.Aux[In, Out2] =
    new Delta.AndThen[In, Out, Out2](out, this, deltaOut2)
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Delta.Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  def from[In] = new From[In]

  class From[In] {
    def curried[Out](f: In => In => Out): Delta.Aux[In, Out] = apply(Function.uncurried(f))
    def apply[Out](f: (In, In) => Out): Delta.Aux[In, Out] = new Function[In, Out](f)
  }

  implicit class DeltaOps[In](val left: In) extends AnyVal {
    def delta(right: In)(implicit delta: Delta[In]): delta.Out = delta(left, right)
  }

  object function {
    implicit def function1Delta[A, B, C](implicit delta: Delta.Aux[B, C]): Delta.Aux[A => B, A => C] = new Delta[A => B] {
      type Out = A => C

      def apply(left: A => B, right: A => B): Out = (a: A) => delta(left(a), right(a))
    }
  }

  object fallback {
    implicit def fallbackDelta[A]: Aux[A, (A, A)] = new Delta[A] {
      type Out = (A, A)

      def apply(left: A, right: A): (A, A) = (left, right)
    }
  }

  private class Function[In, Out0](f: (In, In) => Out0) extends Delta[In] {
    type Out = Out0

    def apply(left: In, right: In): Out = f(left, right)
  }

  private class Mapped[A, B, C](f: B => C, delta: Delta.Aux[A, B]) extends Delta[A] {
    type Out = C

    def apply(left: A, right: A): Out = f(delta(left, right))
  }

  private class Contramapped[A, B, C](f: C => A, delta: Delta.Aux[A, B]) extends Delta[C] {
    type Out = B

    def apply(left: C, right: C): Out = delta(f(left), f(right))
  }

  private class AndThen[In, Out1, Out2](
    out1: Out1, first: Delta.Aux[In, Out1], second: Delta.Aux[Out1, Out2]
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

  def create[P](isEmptyFn: P => Boolean): Patch[P] = new Patch[P] {
    def isEmpty(patch: P): Boolean = isEmptyFn(patch)
  }
}