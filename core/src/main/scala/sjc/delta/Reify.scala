package sjc.delta

import shapeless._

sealed trait Reified {
  def asString: String
}

case class ReifiedValue(value: String) extends Reified {
  def asString: String = value
}

object ReifiedProduct {
  def hlist(values: List[Reified]): Reified = ReifiedProduct(values, "", " :: ", " :: HNil")
}

case class ReifiedProduct(values: List[Reified], start: String, sep: String, end: String) extends Reified {
  def ::(head: Reified): ReifiedProduct = copy(head :: values)
  def asString: String = values.map(_.asString).mkString(start, sep, end)
}

trait Reify[A] {
  def asString(value: A): String = apply(value).asString
  def apply(value: A): Reified
}

object Reify {
  implicit class ReifyOps[A](val value: A) extends AnyVal {
    def reify(implicit reify: Reify[A]): Reified = reify(value)
  }

  def apply[A](implicit reify: Reify[A]): Reify[A] = reify

  implicit val intReify: Reify[Int] = reifyA[Int]

  implicit val stringReify: Reify[String] = reify[String](s ⇒ s""""$s"""")

  implicit def optionReify[A](implicit reifyA: Reify[A]): Reify[Option[A]] =
    reify[Option[A]](_.map(reifyA.asString).toString)

  implicit def eitherReify[L, R](implicit reifyL: Reify[L], reifyR: Reify[R]): Reify[Either[L, R]] =
    reify[Either[L, R]](_.fold(l ⇒ s"Left(${reifyL.asString(l)})", r ⇒ s"Right(${reifyR.asString(r)})"))

  implicit def listReify[A](implicit reifyA: Reify[A]): Reify[List[A]] =
    reify[List[A]](_.map(reifyA.asString).toString)

  implicit def mapReify[K, V](implicit reifyK: Reify[K], reifyV: Reify[V]): Reify[Map[K, V]] =
    reify[Map[K, V]](_.map { case (k, v) ⇒ (reifyK.asString(k), reifyV.asString(v)) }.toString)

  implicit val hnilReify: Reify[HNil] = new Reify[HNil] {
    def apply(value: HNil): Reified = ReifiedProduct.hlist(Nil)
  }

  implicit def hconsReify[H, T <: HList](
    implicit reifyH: Lazy[Reify[H]], reifyT: Lazy[Reify[T]]
  ): Reify[H :: T] = new Reify[H :: T] {
    def apply(value: ::[H, T]): Reified = reifyT.value.apply(value.tail).asInstanceOf[ReifiedProduct] match {
      case rp: ReifiedProduct ⇒ reifyH.value.apply(value.head) :: rp
    }
  }

  implicit def genericReify[In, Repr <: HList](implicit
    gen: Generic.Aux[In, Repr], reify: Lazy[Reify[Repr]]
  ): Reify[In] = new Reify[In] {
    def apply(value: In): Reified = reify.value.apply(gen.to(value)).asInstanceOf[ReifiedProduct] match {
      case rp: ReifiedProduct ⇒ rp.copy(start = value.getClass.getSimpleName + "(", sep = ", ", end = ")")
    }
  }

  private def reifyA[A]: Reify[A] = reify[A](_.toString)

  private def reify[A](f: A ⇒ String): Reify[A] = new Reify[A] {
    def apply(value: A): Reified = ReifiedValue(f(value))
  }
}