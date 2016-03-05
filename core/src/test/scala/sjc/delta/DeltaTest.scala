package sjc.delta

import scala.language.existentials

import org.junit.Test

import scalaz.{Equal, Show}


class DeltaTest extends TestUtil {
  import sjc.delta.Delta._

  @Test def intDeltaTest(): Unit = {
    import sjc.delta.std.int._

    10.delta(2) shouldEqual -8
  }

  @Test def createDeltaFromFunction(): Unit = {
    implicit val doubleDelta = Delta.from[Double] { case (before, after) => after - before }

    1.5.delta(2.0) shouldEqual 0.5

    implicit val stringDelta = Delta.from[String].curried(before => after => (before, after))

    "foo".delta("bar") shouldEqual ("foo", "bar")
  }

  @Test def canMapOverDelta(): Unit = {
    implicit val intDeltaAsString: Delta.Aux[Int, String] = sjc.delta.std.int.deltaInt.map(_.toString)

    1.delta(3) shouldEqual "2"
  }

  @Test def canContramapOverDelta(): Unit = {
    import sjc.delta.std.int._

    implicit val hasIntDelta = Delta[Int].contramap[HasInt](_.i)

    HasInt(1).delta(HasInt(2)) shouldEqual 1.delta(2)
  }

  @Test def fallbackDeltaTest(): Unit = {
    import Delta.fallback._

    HasInt(1).delta(HasInt(2)) shouldEqual (HasInt(1), HasInt(2))
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, Int])
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

  implicit def fallbackEqual[A]: Equal[A] = Equal.equalA[A]
  implicit def fallbackShow[A]: Show[A] = Show.showA[A]
}
