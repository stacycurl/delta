package sjc.delta

import scala.language.implicitConversions


trait Delta[In] {
  type Out

  def apply(before: In, after: In): Out

  def map[B](f: Out => B): Delta.Aux[In, B] = new MappedDelta[In, Out, B](f, this)
  def contramap[B](f: B => In): Delta.Aux[B, Out] = new ContramappedDelta[In, Out, B](f, this)
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Delta.Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  def from[In] = new From[In]

  class From[In] {
    def curried[Out](f: In => In => Out): Delta.Aux[In, Out] = apply(Function.uncurried(f))
    def apply[Out](f: (In, In) => Out): Delta.Aux[In, Out] = new FunctionDelta[In, Out](f)
  }

  implicit class DeltaOps[In](val before: In) extends AnyVal {
    def delta(after: In)(implicit delta: Delta[In]): delta.Out = delta(before, after)
  }

  object fallback {
    implicit def fallbackDelta[A]: Aux[A, (A, A)] = new Delta[A] {
      type Out = (A, A)

      override def apply(before: A, after: A): (A, A) = (before, after)
    }
  }
}

private class FunctionDelta[In, Out0](f: (In, In) => Out0) extends Delta[In] {
  type Out = Out0

  def apply(left: In, right: In): Out = f(left, right)
}

private class MappedDelta[A, B, C](f: B => C, delta: Delta.Aux[A, B]) extends Delta[A] {
  type Out = C

  def apply(left: A, right: A): Out = f(delta(left, right))
}

private class ContramappedDelta[A, B, C](f: C => A, delta: Delta.Aux[A, B]) extends Delta[C] {
  type Out = B

  def apply(left: C, right: C): Out = delta(f(left), f(right))
}

// vim: expandtab:ts=2:sw=2
