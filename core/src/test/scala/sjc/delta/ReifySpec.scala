package sjc.delta

import Reify.ReifyOps
import org.scalatest.{Matchers, FreeSpec}


class ReifySpec extends FreeSpec with Matchers {
  "boolean reify" in {
    false.reify.asString shouldBe "false"
    true.reify.asString shouldBe "true"
  }

  "char reify" in {
    'a'.reify.asString shouldBe "'a'"
  }

  "double reify" in {
    123.456.reify.asString shouldBe "123.456"
  }

  "float reify" in {
    123.456F.reify.asString shouldBe "123.456F"
  }

  "int reify" in {
    123.reify.asString shouldBe "123"
  }

  "long reify" in {
    123L.reify.asString shouldBe "123L"
  }

  "string reify" in {
    "123".reify.asString shouldBe "\"123\""
  }

  "list reify" in {
    List(123).reify.asString shouldBe """List(123)"""
    List("123").reify.asString shouldBe """List("123")"""
  }

  "set reify" in {
    Set(123).reify.asString shouldBe """Set(123)"""
    Set("123").reify.asString shouldBe """Set("123")"""
  }

  "map reify" in {
    Map(123 → List(456)).reify.asString shouldBe """Map(123 -> List(456))"""
    Map("123" → List("456")).reify.asString shouldBe """Map("123" -> List("456"))"""
  }

  "option reify" in {
    (None: Option[Int]).reify.asString shouldBe "None"
    (Some(123): Option[Int]).reify.asString shouldBe """Some(123)"""
    (Some("123"): Option[String]).reify.asString shouldBe """Some("123")"""
  }

  "either reify" in {
    (Left(123): Either[Int, Int]).reify.asString shouldBe """Left(123)"""
    (Left("123"): Either[String, Int]).reify.asString shouldBe """Left("123")"""
    (Right(123): Either[Int, Int]).reify.asString shouldBe """Right(123)"""
    (Right("123"): Either[Int, String]).reify.asString shouldBe """Right("123")"""
  }

  "contramap" in {
    val hasInt: Reify[HasInt] = Reify[Int].contramap[HasInt](_.i)

    hasInt(HasInt(123)).asString shouldBe "HasInt(123)"
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, String])
  case class RP(s: String, o: Option[RP], e: Either[Int, RP])
}
