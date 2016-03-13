package sjc.delta.generic

import scala.language.implicitConversions

import shapeless._
import sjc.delta.Delta


object GenericDelta {
  implicit def genericDelta[In, Repr](
    implicit gen: Generic.Aux[In, Repr], genDelta: Lazy[Delta[Repr]]
  ): Delta.Aux[In, genDelta.value.Out] = new Delta[In] {
    type Out = genDelta.value.Out

    def apply(left: In, right: In): Out =
      genDelta.value(gen.to(left), gen.to(right))
  }

  implicit val hnilDelta: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    type Out = HNil

    def apply(left: HNil, right: HNil): HNil = HNil
  }

  implicit def hconsDelta[H, T <: HList](
    implicit deltaH: Lazy[Delta[H]], deltaT: Lazy[Delta[T] { type Out <: HList }]
  ): Delta.Aux[H :: T, deltaH.value.Out :: deltaT.value.Out] = new Delta[H :: T] {
    type Out = deltaH.value.Out :: deltaT.value.Out

    def apply(left: H :: T, right: H :: T): Out = {
      deltaH.value.apply(left.head, right.head) :: deltaT.value.apply(left.tail, right.tail)
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

    def apply(left: CNil, right: CNil): CNil = left
  }

  implicit def deltaCoproduct[H, T <: Coproduct](
    implicit hDelta: Delta[H], tDelta: Lazy[Delta[T] { type Out <: Coproduct }]
  ): Delta.Aux[H :+: T, CPatch[H, hDelta.Out, T, tDelta.value.Out]] = new Delta[H :+: T] {
    type Out = CPatch[H, hDelta.Out, T, tDelta.value.Out]

    def apply(left: H :+: T, right: H :+: T): CPatch[H, hDelta.Out, T, tDelta.value.Out] = (left, right) match {
      case (Inl(lLeft), Inl(lRight)) => Inl(Inl(hDelta(lLeft, lRight)))
      case (Inr(rLeft), Inr(rRight)) => Inr(tDelta.value(rLeft, rRight))
      case (Inl(lLeft), Inr(rRight)) => Inl(Inr(Inl((lLeft, rRight))))
      case (Inr(rLeft), Inl(lRight)) => Inl(Inr(Inr(Inl((rLeft, lRight)))))
    }
  }

  object deltaPoly extends Poly2 {
    implicit def delta[In](implicit delta: Delta[In]) = at[In, In] {
      case (left, right) => delta(left, right)
    }
  }
}