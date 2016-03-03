package sjc.delta

import scala.reflect.ClassTag


sealed trait Reified {
  def asString: String
}

case class ReifiedValue(value: String) extends Reified {
  def asString: String = value
}

object ReifiedProduct {
  def caseClass(name: String, values: List[Reified]): Reified = ReifiedProduct(values, s"$name(", ", ", ")")
  def hlist(values: List[Reified]): Reified = ReifiedProduct(values, "", " :: ", " :: HNil")
}

case class ReifiedProduct(values: List[Reified], start: String, sep: String, end: String) extends Reified {
  def ::(head: Reified): ReifiedProduct = copy(head :: values)
  def asString: String = values.map(_.asString).mkString(start, sep, end)
}

trait Reify[A] {
  def asString(value: A): String = apply(value).asString
  def apply(value: A): Reified

  def contramap[B](f: B ⇒ A, start: String = null, end: String = ")")(implicit ct: ClassTag[B]): Reify[B] =
    new Reify.Contramapped[A, B](f, this, Option(start).getOrElse(ct.runtimeClass.getSimpleName), end)
}

object Reify {
  implicit class ReifyOps[A](val value: A) extends AnyVal {
    def reify(implicit reify: Reify[A]): Reified = reify(value)
  }

  def apply[A](implicit reify: Reify[A]): Reify[A] = reify

  implicit val booleanReify: Reify[Boolean] = reifyA[Boolean]

  implicit val charReify: Reify[Char] = reify[Char]("'" + _ + "'")

  implicit val doubleReify: Reify[Double] = reifyA[Double]

  implicit val floatReify: Reify[Float] = reify[Float](_ + "F")

  implicit val intReify: Reify[Int] = reifyA[Int]

  implicit val longReify: Reify[Long] = reify[Long](_ + "L")

  implicit val stringReify: Reify[String] = reify[String]("\"" + _ + "\"")

  implicit def optionReify[A](implicit reifyA: Reify[A]): Reify[Option[A]] =
    reify[Option[A]](_.map(reifyA.asString).toString)

  implicit def eitherReify[L, R](implicit reifyL: Reify[L], reifyR: Reify[R]): Reify[Either[L, R]] =
    reify[Either[L, R]](_.fold(l ⇒ s"Left(${reifyL.asString(l)})", r ⇒ s"Right(${reifyR.asString(r)})"))

  implicit def listReify[A](implicit reifyA: Reify[A]): Reify[List[A]] =
    reify[List[A]](_.map(reifyA.asString).toString)

  implicit def setReify[A](implicit reifyA: Reify[A]): Reify[Set[A]] =
    reify[Set[A]](_.map(reifyA.asString).toString)

  implicit def mapReify[K, V](implicit reifyK: Reify[K], reifyV: Reify[V]): Reify[Map[K, V]] =
    reify[Map[K, V]](_.map { case (k, v) ⇒ (reifyK.asString(k), reifyV.asString(v)) }.toString)

  private class Contramapped[A, B](f: B ⇒ A, reifyA: Reify[A], start: String, end: String) extends Reify[B] {
    def apply(value: B): Reified = ReifiedProduct(List(reifyA(f(value))), start + "(", ", ", end)
  }

  def reifyA[A]: Reify[A] = reify[A](_.toString)

  def reify[A](f: A ⇒ String): Reify[A] = reifyIt(a ⇒ ReifiedValue(f(a)))

  def reifyIt[A](f: A ⇒ Reified): Reify[A] = new Reify[A] {
    def apply(value: A): Reified = f(value)
  }
}