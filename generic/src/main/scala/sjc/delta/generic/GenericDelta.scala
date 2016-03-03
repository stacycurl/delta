package sjc.delta.generic

import shapeless._
import sjc.delta.Delta

import scala.language.implicitConversions


object GenericDelta {
  implicit def genericDelta[In, Repr](
    implicit gen: Generic.Aux[In, Repr], genDelta: Lazy[Delta[Repr]]
  ): Delta.Aux[In, genDelta.value.Out] = new Delta[In] {
    type Out = genDelta.value.Out

    def apply(before: In, after: In): Out =
      genDelta.value(gen.to(before), gen.to(after))
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

  object function {
    implicit def function1Delta[A, B](
      implicit delta: Lazy[Delta[B]]
    ): Delta.Aux[A => B, A => delta.value.Out] = new Delta[A => B] {
      type Out = A => delta.value.Out

      override def apply(before: A => B, after: A => B): Out = (a: A) => delta.value(before(a), after(a))
    }
  }
}