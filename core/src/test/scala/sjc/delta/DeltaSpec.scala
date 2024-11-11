package sjc.delta

import scala.language.existentials

import org.scalatest.{Matchers, FreeSpec}


class DeltaSpec extends FreeSpec with Matchers {
  import sjc.delta.Delta._

  "int" in {
    import sjc.delta.std.int._

    10.delta(2) shouldBe -8
  }

  "create delta from function" in {
    implicit val doubleDelta = Delta.from[Double] { case (left, right) => right - left }

    1.5.delta(2.0) shouldBe 0.5

    implicit val stringDelta = Delta.from[String].curried(left => right => (left, right))

    "foo".delta("bar") shouldBe ("foo", "bar")
  }

  "can map over delta" in {
    implicit val intDeltaAsString: Delta.Aux[Int, String] = sjc.delta.std.int.deltaInt.map(_.toString)

    1.delta(3) shouldBe "2"
  }

  "can contramap over delta" in {
    import sjc.delta.std.int._

    implicit val hasIntDelta = Delta[Int].contramap[HasInt](_.i)

    HasInt(1).delta(HasInt(2)) shouldBe 1.delta(2)
  }

  "function" in {
    import sjc.delta.std.int.deltaInt
    import sjc.delta.Delta.function.lift

    val square = (i: Int) => i * i
    val cube = (i: Int) => i * i * i

    val delta = square.delta(cube)

    delta(3) shouldBe(square(3) delta cube(3))

    val x = deltaInt.lift[Int]

    val y = x.apply(square, cube)
  }

  "fallback delta test" in {
    import Delta.fallback._

    HasInt(1).delta(HasInt(2)) shouldBe (HasInt(1), HasInt(2))
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, Int])
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])
}
