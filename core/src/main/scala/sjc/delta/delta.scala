package sjc.delta

import scala.language.implicitConversions

import scalaz.{Equal, Lens, Show, \/, -\/, \/-}
import shapeless._


trait Delta[In] {
  type Out

  def apply(before: In, after: In): Out

  def lens[Container](lens: Lens[Container, In]): Delta.Aux[Container, Out] =
    new LensDelta[Container, In, Out](lens, this)

  def map[B](f: Out => B): Delta.Aux[In, B] = new MappedDelta[In, Out, B](f, this)
}

object Delta {
  def apply[In](implicit delta: Delta[In]): Delta.Aux[In, delta.Out] = delta

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  def from[In] = new From[In]

  class From[In] {
    def apply[Out](f: (In, In) => Out): Delta.Aux[In, Out] = new FunctionDelta[In, Out](f)
  }

  object generic {
    implicit def generic[In, Repr, Out0](
      implicit gen: Generic.Aux[In, Repr], genDelta: Delta.Aux[Repr, Out0]
    ): Delta.Aux[In, Out0] = new Delta[In] {
      type Out = Out0

      def apply(before: In, after: In): Out = genDelta(gen.to(before), gen.to(after))
    }
  }

  implicit class DeltaOps[In](val before: In) extends AnyVal {
    def delta(after: In)(implicit delta: Delta[In]): delta.Out = delta(before, after)
  }

  object std {
    object int {
      implicit val deltaInt: Delta.Aux[Int, Int] = new Delta[Int] {
        type Out = Int

        def apply(before: Int, after: Int): Int = after - before
      }
    }

    object either {
      implicit def deltaEither[L, R, LOut, ROut](
        implicit ldelta: Delta.Aux[L, LOut], rdelta: Delta.Aux[R, ROut]
      ): Delta.Aux[Either[L, R], EitherPatch[L, R, LOut, ROut]] = {
        new Delta[Either[L, R]] {
          type Out = EitherPatch[L, R, LOut, ROut]

          def apply(ebefore: Either[L, R], eafter: Either[L, R]): EitherPatch[L, R, LOut, ROut] = {
            (ebefore, eafter) match {
              case (Left(before),  Left(after))  => BothLeft[LOut](ldelta(before, after))
              case (Right(before), Right(after)) => BothRight[ROut](rdelta(before, after))
              case (Left(before),  Right(after)) => WasLeft(before, after)
              case (Right(before), Left(after))  => WasRight(before, after)
            }
          }
        }
      }

      implicit def deltaV[L, R, LOut, ROut](
        implicit ldelta: Delta.Aux[L, LOut], rdelta: Delta.Aux[R, ROut]
      ): Delta.Aux[\/[L, R], EitherPatch[L, R, LOut, ROut]] = new Delta[L \/ R] {
        type Out = EitherPatch[L, R, LOut, ROut]

        def apply(ebefore: L \/ R, eafter: L \/ R): EitherPatch[L, R, LOut, ROut] = {
          (ebefore, eafter) match {
            case (-\/(before), -\/(after)) => BothLeft[LOut](ldelta(before, after))
            case (\/-(before), \/-(after)) => BothRight[ROut](rdelta(before, after))
            case (-\/(before), \/-(after)) => WasLeft(before, after)
            case (\/-(before), -\/(after)) => WasRight(before, after)
          }
        }
      }

      trait EitherPatch[+L, +R, +LOut, +ROut]
      case class BothLeft[LOut](out: LOut) extends EitherPatch[Nothing, Nothing, LOut, Nothing]
      case class BothRight[ROut](out: ROut) extends EitherPatch[Nothing, Nothing, Nothing, ROut]
      case class WasLeft[L, R](left: L, right: R) extends EitherPatch[L, R, Nothing, Nothing]
      case class WasRight[L, R](right: R, left: L) extends EitherPatch[L, R, Nothing, Nothing]
    }

    object map {
      implicit def deltaMap[K, V, VOut](
        implicit deltaV: Delta.Aux[V, VOut]
      ): Delta.Aux[Map[K, V], MapPatch[K, V, VOut]] = new Delta[Map[K, V]] {
        type Out = MapPatch[K, V, VOut]

        def apply(before: Map[K, V], after: Map[K, V]): MapPatch[K, V, VOut] = {
          val changed: Map[K, VOut] = (before.keySet & after.keySet).map(k => {
            k -> deltaV(before(k), after(k))
          })(scala.collection.breakOut)

          MapPatch[K, V, VOut](after -- before.keySet, before -- after.keySet, changed)
        }
      }

      object equalA {
        implicit def mapPatchEqualA[K, V, VOut]: Equal[MapPatch[K, V, VOut]] =
          Equal.equalA[MapPatch[K, V, VOut]]
      }

      object showA {
        implicit def mapPatchShowA[K, V, VOut]:  Show[MapPatch[K, V, VOut]]  =
          Show.showA[MapPatch[K, V, VOut]]
      }

      case class MapPatch[K, V, VOut](added: Map[K, V], removed: Map[K, V], changed: Map[K, VOut])
    }

    object set {
      implicit def deltaSet[A]: Delta.Aux[Set[A], SetPatch[A]] = new Delta[Set[A]] {
        type Out = SetPatch[A]

        def apply(before: Set[A], after: Set[A]): SetPatch[A] =
          SetPatch[A](added = after -- before, removed = before -- after)
      }

      object equalA {
        implicit def setPatchEqualA[A]: Equal[SetPatch[A]] = Equal.equalA[SetPatch[A]]
      }

      object showA {
        implicit def setPatchShowA[A]: Show[SetPatch[A]]   = Show.showA[SetPatch[A]]
      }

      case class SetPatch[A](added: Set[A], removed: Set[A])
    }
  }

  object hlist {
    implicit val hnilDelta: Delta.Aux[HNil, HNil] = new Delta[HNil] {
      type Out = HNil

      def apply(before: HNil, after: HNil): HNil = HNil
    }

    object deltaPoly extends Poly2 {
      implicit def delta[In](implicit delta: Delta[In]) = at[In, In] {
        case (before, after) => delta(before, after)
      }
    }

    implicit def hconsDelta[H, T <: HList](
      implicit deltaH: Delta[H], deltaT: Lazy[Delta[T] { type Out <: HList }]
    ): Delta.Aux[H :: T, deltaH.Out :: deltaT.value.Out] = new Delta[H :: T] {
      type Out = deltaH.Out :: deltaT.value.Out

      def apply(before: H :: T, after: H :: T): Out = {
        deltaH.apply(before.head, after.head) :: deltaT.value.apply(before.tail, after.tail)
      }
    }
  }

  object coproduct {
    private type CPatch[H, HOut, T <: Coproduct, TOut <: Coproduct] =
      (HOut :+: (H, T) :+: (T, H) :+: CNil) :+: TOut

    implicit val deltaCNil: Delta.Aux[CNil, CNil] = new Delta[CNil] {
      type Out = CNil

      def apply(before: CNil, after: CNil): CNil = before
    }

    implicit def deltaCoproduct[H, T <: Coproduct, HOut, TOut <: Coproduct](
      implicit hDelta: Delta.Aux[H, HOut], tDelta: Delta.Aux[T, TOut]
    ): Delta.Aux[H :+: T, CPatch[H, HOut, T, TOut]] = new Delta[H :+: T] {
      type Out = CPatch[H, HOut, T, TOut]

      def apply(before: H :+: T, after: H :+: T): CPatch[H, HOut, T, TOut] = (before, after) match {
        case (Inl(lBefore), Inl(lAfter)) => Inl(Inl(hDelta(lBefore, lAfter)))
        case (Inr(rBefore), Inr(rAfter)) => Inr(tDelta(rBefore, rAfter))
        case (Inl(lBefore), Inr(rAfter)) => Inl(Inr(Inl((lBefore, rAfter))))
        case (Inr(rBefore), Inl(lAfter)) => Inl(Inr(Inr(Inl((rBefore, lAfter)))))
      }
    }
  }
}

private class LensDelta[Container, In, Out0](lens: Lens[Container, In], delta: Delta.Aux[In, Out0])
  extends Delta[Container] {

  type Out = Out0

  def apply(left: Container, right: Container): Out = delta(lens.get(left), lens.get(right))
}

private class FunctionDelta[In, Out0](f: (In, In) => Out0) extends Delta[In] {
  type Out = Out0

  def apply(left: In, right: In): Out = f(left, right)
}

private class MappedDelta[A, B, C](f: B => C, delta: Delta.Aux[A, B]) extends Delta[A] {
  type Out = C

  def apply(left: A, right: A): Out = f(delta(left, right))
}

// vim: expandtab:ts=2:sw=2
