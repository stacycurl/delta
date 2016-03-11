package sjc.delta

import scala.language.implicitConversions


trait Delta[In] {
  type Out

  def apply(left: In, right: In): Out

  def map[B](f: Out => B): Delta.Aux[In, B] = new Delta.Mapped[In, Out, B](f, this)
  def contramap[B](f: B => In): Delta.Aux[B, Out] = new Delta.Contramapped[In, Out, B](f, this)
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

  object fallback {
    implicit def fallbackDelta[A]: Aux[A, (A, A)] = new Delta[A] {
      type Out = (A, A)

      override def apply(left: A, right: A): (A, A) = (left, right)
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
}


trait DeltaWithZero[In] extends Delta[In] {
  def zero: Out

  override def map[Out2](f: Out ⇒ Out2): DeltaWithZero.Aux[In, Out2] =
    new DeltaWithZero.Mapped[In, Out, Out2](f, this)

  override def contramap[In2](f: In2 ⇒ In): DeltaWithZero.Aux[In2, Out] =
    new DeltaWithZero.Contramapped[In, Out, In2](f, this)
}

object DeltaWithZero {
  type Aux[A, Out0] = DeltaWithZero[A] {
    type Out = Out0
  }

  def apply[A](implicit deltaWithZero: DeltaWithZero[A]): Aux[A, deltaWithZero.Out] = deltaWithZero

  def from[In] = new From[In]

  class From[In] {
    def apply[Out0](z: Out0, f: (In, In) => Out0): DeltaWithZero.Aux[In, Out0] = new DeltaWithZero[In] {
      type Out = Out0
      val zero: Out0 = z
      def apply(left: In, right: In): Out0 = f(left, right)
    }
  }

  private class Mapped[In, Out, Out2](f: Out ⇒ Out2, deltaWithZero: Aux[In, Out]) extends DeltaWithZero[In] {
    type Out = Out2

    def zero: Out2 = f(deltaWithZero.zero)
    def apply(left: In, right: In): Out2 = f(deltaWithZero.apply(left, right))
  }

  private class Contramapped[In, Out0, In2](f: In2 ⇒ In, deltaWithZero: Aux[In, Out0]) extends DeltaWithZero[In2] {
    type Out = Out0

    def zero: Out0 = deltaWithZero.zero
    def apply(left: In2, right: In2): Out0 = deltaWithZero(f(left), f(right))
  }
}