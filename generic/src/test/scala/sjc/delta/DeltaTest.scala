package sjc.delta

import scala.language.existentials

import org.junit.Test

import scalaz.{Equal, Show, \/}
import shapeless._

import scalaz.std.either._
import scalaz.std.AllInstances._
import sjc.delta.generic.GenericDelta.deltaPoly
//import sjc.delta.generic.GenericDelta.function._



class DeltaTest extends TestUtil {
  import sjc.delta.Delta._

  @Test def hlistDeltaTest(): Unit ={
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.{hnilDelta, hconsDelta}

    (1 :: 10 :: HNil).delta(3 :: 30 :: HNil) shouldEqual(1.delta(3) :: 10.delta(30) :: HNil)

    (1 :: 10 :: HNil).zipWith(3 :: 30 :: HNil)(deltaPoly) shouldEqual(1.delta(3) :: 10.delta(30) :: HNil)
  }

  @Test def coproductDeltaTest(): Unit = {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.{deltaCNil, deltaCoproduct}


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
    import sjc.delta.generic.GenericDelta._

    HasInt(1).delta(HasInt(2)) shouldEqual(1.delta(2) :: HNil)

    val actual = RecursiveProduct(1, None).delta(RecursiveProduct(3, None))

    val expected: ::[Aux[Int, Int]#Out, ::[Inl[Inl[HNil.type, Nothing], Nothing], HNil]] = 1.delta(3) :: Inl(Inl(HNil)) :: HNil

    actual shouldEqual expected

    // Doesn't work yet
//    RecursiveProduct(1, Some(RecursiveProduct(10, None))).delta(RecursiveProduct(3, Some(RecursiveProduct(30, None))))
//      .must(equal(1.delta(3) :: Inl(Inl(10.delta(30)) :: Inl(Inl(HNil)) :: HNil) :: HNil))
  }


  @Test def functionDeltaTest(): Unit = {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.function.function1Delta

    val square = (i: Int) => i * i
    val cube = (i: Int) => i * i * i

    val delta = square.delta(cube)

    delta(3) shouldEqual(27 - 9)
  }

  case class HasInt(i: Int)
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

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
