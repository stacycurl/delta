package sjc.delta

import org.specs2.scalaz.{ScalazMatchers, Spec}
import shapeless.HNil

import scalaz.\/


class ReifySpec extends Spec with ScalazMatchers {
  import scalaz.std.AllInstances._
  import Reify._

  "int reify" in {
    123.reify.asString must equal("123")
  }

  "string reify" in {
    "123".reify.asString must equal("\"123\"")
  }

  "list reify" in {
    List(123).reify.asString must equal("""List(123)""")
    List("123").reify.asString must equal("""List("123")""")
  }

  "map reify" in {
    Map(123 -> List(456)).reify.asString must equal("""Map(123 -> List(456))""")
    Map("123" -> List("456")).reify.asString must equal("""Map("123" -> List("456"))""")
  }

  "option reify" in {
    (None: Option[Int]).reify.asString must equal("None")
    (Some(123): Option[Int]).reify.asString must equal("""Some(123)""")
    (Some("123"): Option[String]).reify.asString must equal("""Some("123")""")
  }

  "either reify" in {
    (Left(123): Either[Int, Int]).reify.asString must equal("""Left(123)""")
    (Left("123"): Either[String, Int]).reify.asString must equal("""Left("123")""")
    (Right(123): Either[Int, Int]).reify.asString must equal("""Right(123)""")
    (Right("123"): Either[Int, String]).reify.asString must equal("""Right("123")""")
  }

  "hlist reify" in {
    (123 :: "123" :: HNil).reify.asString must equal("""123 :: "123" :: HNil""")
  }

  "generic reify" in {
    HasInt(123).reify.asString must equal("""HasInt(123)""")
    MapAndInt(123, Map(123 -> "456")).reify.asString must equal("""MapAndInt(123, Map(123 -> "456"))""")

    RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777)))).reify.asString must equal(
      """RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777))))"""
    )
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, String])
  case class RP(s: String, o: Option[RP], e: Either[Int, RP])
}
