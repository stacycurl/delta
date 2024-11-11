package sjc.delta.generic

import shapeless._
import sjc.delta.{Reified, ReifiedProduct, Reify}


object GenericReify {
  implicit val hnilReify: Reify[HNil] = new Reify[HNil] {
    def apply(value: HNil): Reified = ReifiedProduct.hlist(Nil)
  }

  implicit def hconsReify[H, T <: HList](
    implicit reifyH: Lazy[Reify[H]], reifyT: Lazy[Reify[T]]
  ): Reify[H :: T] = new Reify[H :: T] {
    def apply(value: ::[H, T]): Reified = reifyT.value.apply(value.tail).asInstanceOf[ReifiedProduct] match {
      case rp: ReifiedProduct => reifyH.value.apply(value.head) :: rp
    }
  }

  implicit def genericReify[In, Repr <: HList](implicit
    gen: Generic.Aux[In, Repr], reify: Lazy[Reify[Repr]]
  ): Reify[In] = new Reify[In] {
    def apply(value: In): Reified = reify.value.apply(gen.to(value)).asInstanceOf[ReifiedProduct] match {
      case rp: ReifiedProduct => rp.copy(start = value.getClass.getSimpleName + "(", sep = ", ", end = ")")
    }
  }
}