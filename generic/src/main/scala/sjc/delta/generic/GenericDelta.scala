package sjc.delta.generic

import scala.language.implicitConversions

import shapeless._
import sjc.delta.{Patch, Delta}

import scala.reflect.ClassTag


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

  implicit def hnilPatch[Path]: Patch[HNil, Path] = new Patch[HNil, Path] {
    def isEmpty(patch: HNil): Boolean = true

    def pretty(p: HNil): String = "HNil"

    def ignore(p: HNil, paths: Path*): HNil = p

    protected val classTag: ClassTag[HNil] = implicitly[ClassTag[HNil]]
  }

  implicit def hconsPatch[H, T <: HList](
    implicit patchH: Lazy[Patch[H, Unit]], patchT: Lazy[Patch[T, Unit]]
  ): Patch[H :: T, Unit] = new Patch[H :: T, Unit] {
    def isEmpty(patch: H :: T): Boolean = patchH.value.isEmpty(patch.head) && patchT.value.isEmpty(patch.tail)

    def pretty(p: H :: T): String = p.toString

    def ignore(p: H :: T, paths: Unit*): H :: T = p

    protected val classTag: ClassTag[H :: T] = implicitly[ClassTag[H :: T]]
  }
  /*
    implicit def optionDelta[T](
      implicit deltaT: Lazy[Delta[T]]
    ): Delta.Aux[Option[T], Option[deltaT.value.Out] :+: T :+: T :+: CNil] = new Delta[Option[T]] {
      type Out = Option[deltaT.value.Out] :+: T :+: T :+: CNil

      def apply(before: Option[T], after: Option[T]): Out = (before, after) match {
        case (None, None)       ⇒ Inl(None)
        case (Some(b), Some(a)) ⇒ Inl(Some(deltaT.value.apply(b, a)))
        case (Some(b), None)    ⇒ Inr(Inl(b))
        case (None, Some(a))    ⇒ Inr(Inr(Inl(a)))
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
      case (Inl(lLeft), Inl(lRight)) ⇒ Inl(Inl(hDelta(lLeft, lRight)))
      case (Inr(rLeft), Inr(rRight)) ⇒ Inr(tDelta.value(rLeft, rRight))
      case (Inl(lLeft), Inr(rRight)) ⇒ Inl(Inr(Inl((lLeft, rRight))))
      case (Inr(rLeft), Inl(lRight)) ⇒ Inl(Inr(Inr(Inl((rLeft, lRight)))))
    }
  }

  object deltaPoly extends Poly2 {
    implicit def delta[In](implicit delta: Delta[In]) = at[In, In] {
      case (left, right) ⇒ delta(left, right)
    }
  }
}

object GenericSymbolDelta {
  import shapeless._, labelled.{ field, FieldType }, syntax.singleton._

  implicit val hnilDelta: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    type Out = HNil

    def apply(left: HNil, right: HNil): HNil = HNil
  }

  implicit def hconsDelta[K <: Symbol, V, T <: HList](
    implicit key: Witness.Aux[K], deltaV: Lazy[Delta[V]], deltaT: Lazy[Delta[T] { type Out <: HList }]
  ): Delta[FieldType[K, V] :: T] = new Delta[FieldType[K, V] :: T] {
    type Out = (String, deltaV.value.Out) :: deltaT.value.Out

    def apply(left: FieldType[K, V] :: T, right: FieldType[K, V] :: T): Out = {
      val head = (key.value.name → deltaV.value.apply(left.head, right.head))
      val tail = deltaT.value.apply(left.tail, right.tail)

      head :: tail
    }
  }

  implicit val cnilDelta: Delta.Aux[CNil, CNil] = new Delta[CNil] {
    type Out = CNil

    def apply(left: CNil, right: CNil): CNil = left
  }

  implicit def cconsDelta[K <: Symbol, V, T <: Coproduct](
    implicit key: Witness.Aux[K], deltaV: Lazy[Delta[V]], deltaT: Lazy[Delta[T] { type Out <: Coproduct }]
  ): Delta[FieldType[K, V] :+: T] = new Delta[FieldType[K, V] :+: T] {
    type Out = (String, deltaV.value.Out) :+: deltaT.value.Out

    def apply(left: FieldType[K, V] :+: T, right: FieldType[K, V] :+: T): Out = (left, right) match {
      case (Inl(lLeft), Inl(lRight)) ⇒ sys.error("inl inl")
      case (Inr(rLeft), Inr(rRight)) ⇒ sys.error("inr inr")
      case (Inl(lLeft), Inr(rRight)) ⇒ sys.error("inl inr")
      case (Inr(rLeft), Inl(lRight)) ⇒ sys.error("inr inl")
    }
  }

  implicit def genericDelta[F, G](
    implicit gen: LabelledGeneric.Aux[F, G], deltaG: Lazy[Delta[G]]
  ): Delta[F] = new Delta[F] {
    type Out = deltaG.value.Out

    def apply(left: F, right: F): Out = {
      deltaG.value.apply(gen.to(left), gen.to(right))
    }
  }
}