package sjc.delta

import org.junit.Test
import scala.collection.{mutable ⇒ M}

import org.junit.Assert._

import shapeless.HNil

import scalaz.\/


class ReifyTest {
  import scalaz.std.AllInstances._
  import SpecyOps._
  import Reify._

  @Test def `boolean reify`(): Unit = {
    false.reify.asString shouldEqual "false"
    true.reify.asString shouldEqual "true"
  }

  @Test def `int reify`(): Unit = {
    123.reify.asString shouldEqual "123"
  }

  @Test def `string reify`(): Unit = {
    "123".reify.asString shouldEqual "\"123\""
  }

  @Test def `list reify`(): Unit = {
    List(123).reify.asString shouldEqual """List(123)"""
    List("123").reify.asString shouldEqual """List("123")"""
  }

  @Test def `set reify`(): Unit = {
    Set(123).reify.asString shouldEqual """Set(123)"""
    Set("123").reify.asString shouldEqual """Set("123")"""
  }

  @Test def `map reify`(): Unit = {
    Map(123 -> List(456)).reify.asString shouldEqual """Map(123 -> List(456))"""
    Map("123" -> List("456")).reify.asString shouldEqual """Map("123" -> List("456"))"""
  }

  @Test def `option reify`(): Unit = {
    (None: Option[Int]).reify.asString shouldEqual "None"
    (Some(123): Option[Int]).reify.asString shouldEqual """Some(123)"""
    (Some("123"): Option[String]).reify.asString shouldEqual """Some("123")"""
  }

  @Test def `either reify`(): Unit = {
    (Left(123): Either[Int, Int]).reify.asString shouldEqual """Left(123)"""
    (Left("123"): Either[String, Int]).reify.asString shouldEqual """Left("123")"""
    (Right(123): Either[Int, Int]).reify.asString shouldEqual """Right(123)"""
    (Right("123"): Either[Int, String]).reify.asString shouldEqual """Right("123")"""
  }

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

  @Test def contramap(): Unit = {
    val hasInt: Reify[HasInt] = Reify[Int].contramap[HasInt](_.i)

    hasInt(HasInt(123)).asString shouldEqual "HasInt(123)"
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, String])
  case class RP(s: String, o: Option[RP], e: Either[Int, RP])
}
