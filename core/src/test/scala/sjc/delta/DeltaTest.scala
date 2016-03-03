package sjc.delta

import scala.language.existentials

import org.junit.Test

import scalaz.{Equal, Show, \/}
import shapeless._

import scalaz.std.either._


class DeltaTest {
  import SpecyOps._

  import scalaz.std.AllInstances._
  import sjc.delta.Delta._

  @Test def intDeltaTest(): Unit = {
    import sjc.delta.std.int._

    10.delta(2) shouldEqual -8
  }


  @Test def setDeltaTest(): Unit ={
    import sjc.delta.std.set._

    val expected = SetPatch(removed = Set(1), added = Set(3))

    Set(1, 2).delta(Set(2, 3)) shouldEqual expected
  }

  @Test def mapDeltaTest(): Unit ={
    import sjc.delta.std.int._
    import sjc.delta.std.map._

    beforeM.delta(afterM) shouldEqual expectedM

    val nested = Map("a" -> Map(1 -> 1), "b" -> beforeM).delta(Map("b" -> afterM, "c" -> Map(3 -> 3)))

    nested shouldEqual MapPatch(
      added   = Map("c" -> Map(3 -> 3)),
      removed = Map("a" -> Map(1 -> 1)),
      changed = Map("b" -> expectedM)
    )
  }

  @Test def hlistDeltaTest(): Unit ={
    import sjc.delta.std.int._

    (1 :: 10 :: HNil).delta(3 :: 30 :: HNil) shouldEqual(1.delta(3) :: 10.delta(30) :: HNil)

    (1 :: 10 :: HNil).zipWith(3 :: 30 :: HNil)(deltaPoly) shouldEqual(1.delta(3) :: 10.delta(30) :: HNil)
  }

  @Test def coproductDeltaTest(): Unit = {
    import sjc.delta.std.int._

    type CPatch[H, T <: Coproduct] = H :+: (H, T) :+: (T, H) :+: CNil

    type E  = Int :+: Int :+: Int :+: CNil
    type EP = CPatch[Int, Int :+: Int :+: CNil] :+: CPatch[Int, Int :+: CNil] :+: CPatch[Int, CNil] :+: CNil

    (Inl(2): E).delta(Inl(10): E)           shouldEqual(Inl(Inl(8)): EP)
    (Inr(Inl(10)): E).delta(Inr(Inl(2)): E) shouldEqual(Inr(Inl(Inl((-8)))): EP)
    (Inl(2): E).delta(Inr(Inl(10)): E)      shouldEqual(Inl(Inr(Inl((2, Inl(10))))): EP)
    (Inr(Inl(2)): E).delta(Inl(10): E)      shouldEqual(Inl(Inr(Inr(Inl((Inl(2), 10))))): EP)
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

  @Test def genericDeltaTest(): Unit = {
    import sjc.delta.std.int._
    import sjc.delta.std.map._

    HasInt(1).delta(HasInt(2)) shouldEqual(1.delta(2) :: HNil)
    MapAndInt(1, beforeM).delta(MapAndInt(2, afterM)) shouldEqual(1.delta(2) :: expectedM :: HNil)

    val actual = RecursiveProduct(1, None).delta(RecursiveProduct(3, None))

    val expected: ::[Aux[Int, Int]#Out, ::[Inl[Inl[HNil.type, Nothing], Nothing], HNil]] = 1.delta(3) :: Inl(Inl(HNil)) :: HNil

    actual shouldEqual expected

    // Doesn't work yet
//    RecursiveProduct(1, Some(RecursiveProduct(10, None))).delta(RecursiveProduct(3, Some(RecursiveProduct(30, None))))
//      .must(equal(1.delta(3) :: Inl(Inl(10.delta(30)) :: Inl(Inl(HNil)) :: HNil) :: HNil))
  }


  @Test def functionDeltaTest(): Unit = {
    import sjc.delta.std.int._
    import sjc.delta.Delta.function._

    val square = (i: Int) => i * i
    val cube = (i: Int) => i * i * i

    val delta = square.delta(cube)

    delta(3) shouldEqual(27 - 9)
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, Int])
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

  val beforeM = Map(1 -> 1, 2 -> 2)
  val afterM  = Map(2 -> 22, 3 -> 3)

  val expectedM = sjc.delta.std.map.MapPatch(
    added   = Map(3 -> 3),
    removed = Map(1 -> 1),
    changed = Map(2 -> sjc.delta.std.int.deltaInt(2, 22))
  )

  implicit def coproductEqual[C <: Coproduct]: Equal[C] = Equal.equalA[C]

  implicit def coproductShow[C0 <: Coproduct]: Show[C0] = Show.show[C0](c0 => {
    def recurse[C <: Coproduct](c: C): String = c match {
      case Inl(head) => s"Inl(${head})"
      case Inr(tail) => s"Inr(${recurse(tail)})"
    }

    recurse(c0)
  })

  implicit def hlistEqual[L <: HList]: Equal[L] = Equal.equalA[L]
  implicit def hlistShow[L <: HList]: Show[L] = Show.showA[L]

  implicit def fallbackEqual[A]: Equal[A] = Equal.equalA[A]
  implicit def fallbackShow[A]: Show[A] = Show.showA[A]
}
