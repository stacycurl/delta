package sjc.delta

import org.junit.Test
import shapeless.HNil

import sjc.delta.Reify._
import sjc.delta.generic.GenericReify._


class ReifyTest extends TestUtil {
  @Test def `hlist reify`(): Unit = {
    (123 :: "123" :: HNil).reify.asString shouldEqual """123 :: "123" :: HNil"""
  }

  @Test def `generic reify`(): Unit = {
    HasInt(123).reify.asString shouldEqual """HasInt(123)"""
    MapAndInt(123, Map(123 -> "456")).reify.asString shouldEqual """MapAndInt(123, Map(123 -> "456"))"""

    RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777)))).reify.asString shouldEqual(
      """RP("123", Some(RP("456", None, Left(444))), Right(RP("789", None, Left(777))))"""
    )
  }

  @Test def `manual case class reify`(): Unit = {
    ReifiedProduct.caseClass("Fred", List(ReifiedValue("123"))).asString shouldEqual "Fred(123)"
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, String])
  case class RP(s: String, o: Option[RP], e: Either[Int, RP])
}
