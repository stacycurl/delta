package sjc.delta

import scala.language.existentials

import org.scalatest.{Matchers, FreeSpec}

import shapeless._

import sjc.delta.generic.GenericDelta.deltaPoly


class DeltaTest extends FreeSpec with Matchers {
  import sjc.delta.Delta._

  "hlist" in {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.{hnilDelta, hconsDelta}

    (1 :: 10 :: HNil).delta(3 :: 30 :: HNil) shouldBe(1.delta(3) :: 10.delta(30) :: HNil)

    (1 :: 10 :: HNil).zipWith(3 :: 30 :: HNil)(deltaPoly) shouldBe(1.delta(3) :: 10.delta(30) :: HNil)
  }

  "coproduct" in {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.{deltaCNil, deltaCoproduct}


    type CPatch[H, T <: Coproduct] = H :+: (H, T) :+: (T, H) :+: CNil

    type E  = Int :+: Int :+: Int :+: CNil
    type EP = CPatch[Int, Int :+: Int :+: CNil] :+: CPatch[Int, Int :+: CNil] :+: CPatch[Int, CNil] :+: CNil

    (Inl(2): E).delta(Inl(10): E)           shouldBe(Inl(Inl(8)): EP)
    (Inr(Inl(10)): E).delta(Inr(Inl(2)): E) shouldBe(Inr(Inl(Inl((-8)))): EP)
    (Inl(2): E).delta(Inr(Inl(10)): E)      shouldBe(Inl(Inr(Inl((2, Inl(10))))): EP)
    (Inr(Inl(2)): E).delta(Inl(10): E)      shouldBe(Inl(Inr(Inr(Inl((Inl(2), 10))))): EP)
  }

  "generic" in {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta._

    HasInt(1).delta(HasInt(2)) shouldBe(1.delta(2) :: HNil)

    val actual = RecursiveProduct(1, None).delta(RecursiveProduct(3, None))

    val expected: ::[Aux[Int, Int]#Out, ::[Inl[Inl[HNil.type, Nothing], Nothing], HNil]] = 1.delta(3) :: Inl(Inl(HNil)) :: HNil

    actual shouldBe expected

    // Doesn't work yet
//    RecursiveProduct(1, Some(RecursiveProduct(10, None))).delta(RecursiveProduct(3, Some(RecursiveProduct(30, None))))
//      .must(equal(1.delta(3) :: Inl(Inl(10.delta(30)) :: Inl(Inl(HNil)) :: HNil) :: HNil))
  }

  "function" in {
    import sjc.delta.std.int._
    import sjc.delta.generic.GenericDelta.function.function1Delta

    val square = (i: Int) => i * i
    val cube = (i: Int) => i * i * i

    val delta = square.delta(cube)

    delta(3) shouldBe(27 - 9)
  }

  case class HasInt(i: Int)
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

/*
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
  */
}
