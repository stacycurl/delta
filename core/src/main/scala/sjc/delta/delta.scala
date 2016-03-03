package sjc.delta

import scala.annotation.tailrec
import scala.language.implicitConversions

import java.util.concurrent.atomic.AtomicInteger
import scala.xml._

import shapeless._


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

  implicit def generic[In, Repr](
    implicit gen: Generic.Aux[In, Repr], genDelta: Lazy[Delta[Repr]]
  ): Delta.Aux[In, genDelta.value.Out] = new Delta[In] {
    type Out = genDelta.value.Out

    def apply(before: In, after: In): Out =
      genDelta.value(gen.to(before), gen.to(after))
  }

  object function {
    implicit def function1Delta[A, B](
      implicit delta: Lazy[Delta[B]]
    ): Delta.Aux[A => B, A => delta.value.Out] = new Delta[A => B] {
      type Out = A => delta.value.Out

      override def apply(before: A => B, after: A => B): Out = (a: A) => delta.value(before(a), after(a))
    }
  }

  implicit val hnilDelta: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    type Out = HNil

    def apply(before: HNil, after: HNil): HNil = HNil
  }

  implicit def hconsDelta[H, T <: HList](
    implicit deltaH: Lazy[Delta[H]], deltaT: Lazy[Delta[T] { type Out <: HList }]
  ): Delta.Aux[H :: T, deltaH.value.Out :: deltaT.value.Out] = new Delta[H :: T] {
    type Out = deltaH.value.Out :: deltaT.value.Out

    def apply(before: H :: T, after: H :: T): Out = {
      deltaH.value.apply(before.head, after.head) :: deltaT.value.apply(before.tail, after.tail)
    }
  }
/*
  implicit def optionDelta[T](
    implicit deltaT: Lazy[Delta[T]]
  ): Delta.Aux[Option[T], Option[deltaT.value.Out] :+: T :+: T :+: CNil] = new Delta[Option[T]] {
    type Out = Option[deltaT.value.Out] :+: T :+: T :+: CNil

    def apply(before: Option[T], after: Option[T]): Out = (before, after) match {
      case (None, None)       => Inl(None)
      case (Some(b), Some(a)) => Inl(Some(deltaT.value.apply(b, a)))
      case (Some(b), None)    => Inr(Inl(b))
      case (None, Some(a))    => Inr(Inr(Inl(a)))
    }
  }
*/
  private type CPatch[H, HOut, T <: Coproduct, TOut <: Coproduct] =
    (HOut :+: (H, T) :+: (T, H) :+: CNil) :+: TOut

  implicit val deltaCNil: Delta.Aux[CNil, CNil] = new Delta[CNil] {
    type Out = CNil

    def apply(before: CNil, after: CNil): CNil = before
  }

  implicit def deltaCoproduct[H, T <: Coproduct](
    implicit hDelta: Delta[H], tDelta: Lazy[Delta[T] { type Out <: Coproduct }]
  ): Delta.Aux[H :+: T, CPatch[H, hDelta.Out, T, tDelta.value.Out]] = new Delta[H :+: T] {
    type Out = CPatch[H, hDelta.Out, T, tDelta.value.Out]

    def apply(before: H :+: T, after: H :+: T): CPatch[H, hDelta.Out, T, tDelta.value.Out] = (before, after) match {
      case (Inl(lBefore), Inl(lAfter)) => Inl(Inl(hDelta(lBefore, lAfter)))
      case (Inr(rBefore), Inr(rAfter)) => Inr(tDelta.value(rBefore, rAfter))
      case (Inl(lBefore), Inr(rAfter)) => Inl(Inr(Inl((lBefore, rAfter))))
      case (Inr(rBefore), Inl(lAfter)) => Inl(Inr(Inr(Inl((rBefore, lAfter)))))
    }
  }

  object deltaPoly extends Poly2 {
    implicit def delta[In](implicit delta: Delta[In]) = at[In, In] {
      case (before, after) => delta(before, after)
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
