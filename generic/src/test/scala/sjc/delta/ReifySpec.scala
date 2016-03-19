package sjc.delta

import org.scalatest.{Matchers, FreeSpec}
import shapeless.HNil

import sjc.delta.Reify._
import sjc.delta.generic.GenericReify._


class ReifySpec extends FreeSpec with Matchers {
  "hlist reify" in {
    (123 :: "123" :: HNil).reify.asString shouldBe """123 :: "123" :: HNil"""
  }

  "generic reify" in {
    HasInt(123).reify.asString shouldBe """HasInt(123)"""
    MapAndInt(123, Map(123 → "456")).reify.asString shouldBe """MapAndInt(123, Map(123 -> "456"))"""

    RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777)))).reify.asString shouldBe(
      """RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777))))"""
    )
  }

  "manual case class reify" in {
    ReifiedProduct.caseClass("Fred", List(ReifiedValue("123"))).asString shouldBe "Fred(123)"
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, String])
  case class RP(s: String, o: Option[RP], e: Either[Int, RP])
}
