package sjc.delta

import sjc.delta.std.list.naive.{Extra, Missing, Diff, Change}

import scala.language.existentials

import org.scalatest.{Matchers, FreeSpec}

import shapeless._

import scala.reflect.ClassTag


class DeltaSpec extends FreeSpec with Matchers {
  import sjc.delta.Delta._
  import sjc.delta.generic.GenericDelta.deltaPoly

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

  "abc" - {
    case class A(bs: List[B])
    case class B(cs: List[C])
    case class C(v: Int)

    implicit val deltaC: Delta.Aux[C, Int] = sjc.delta.std.int.deltaInt.contramap[C](_.v)

    "patience" in {
      import sjc.delta.std.list.patience.deltaList

      println(List(
        A(List(
          B(List(C(111), C(222))),
          B(List(C(333), C(444)))
        )),
        A(List(
          B(List(C(555), C(666))),
          B(List(C(777), C(888)))
        ))
      ) delta List(
        A(List(
          B(List(C(1))),
          B(List(C(333), C(44), C(444)))
        )),
        A(List(
          B(List(C(555), C(666), C(66)))
        ))
      ))
    }

List(
  Diff(0, List(
    Diff(0, List(Diff(0,-110), Missing(1, C(222)))),
    Diff(1, List(Diff(1,-400), Extra(2, C(444))))
  )),
  Diff(1, List(
    Diff(0, List(Extra(2, C(66)))),
    Missing(1, B(List(C(777), C(888))))
  ))
)

    "naive" in {
import sjc.delta.std.list.naive.deltaList

implicit val deltaC: Delta.Aux[C, Int] = sjc.delta.std.int.deltaInt.contramap[C](_.v)
implicit val patchInt: Patch[Int, Unit] = sjc.delta.std.int.deltaInt

implicit val deltaListC: Aux[List[C], List[Change[C, Int]]] = deltaList(deltaC, patchInt)

implicit def patchList[A: ClassTag]: Patch[List[A], Unit] = new Patch[List[A], Unit] {
  def isEmpty(patch: List[A]): Boolean = patch.isEmpty
  def pretty(p: List[A]): String = p.toString
  def ignore(p: List[A], paths: Unit*): List[A] = p

  protected val classTag: ClassTag[List[A]] = implicitly[ClassTag[List[A]]]
}

implicit val deltaB: Delta.Aux[B, List[Change[C, Int]]] = Delta[List[C]].contramap[B](_.cs)

implicit val deltaListB = deltaList(deltaB, patchList[Change[C, Int]])

implicit val deltaA = deltaListB.contramap[A](_.bs)

implicit val deltaListA = deltaList(deltaA, patchList[Change[B, List[Change[C, Int]]]])

println(List(
  A(List(
    B(List(C(111), C(222))),
    B(List(C(333), C(444)))
  )),
  A(List(
    B(List(C(555), C(666))),
    B(List(C(777), C(888)))
  ))
) delta List(
  A(List(
    B(List(C(1))),
    B(List(C(333), C(44), C(444)))
  )),
  A(List(
    B(List(C(555), C(666), C(66)))
  ))
))
    }
  }

  case class HasInt(i: Int)
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

/*
  implicit def coproductEqual[C <: Coproduct]: Equal[C] = Equal.equalA[C]

  implicit def coproductShow[C0 <: Coproduct]: Show[C0] = Show.show[C0](c0 ⇒ {
    def recurse[C <: Coproduct](c: C): String = c match {
      case Inl(head) ⇒ s"Inl(${head})"
      case Inr(tail) ⇒ s"Inr(${recurse(tail)})"
    }

    recurse(c0)
  })

  implicit def hlistEqual[L <: HList]: Equal[L] = Equal.equalA[L]
  implicit def hlistShow[L <: HList]: Show[L] = Show.showA[L]

  implicit def fallbackEqual[A]: Equal[A] = Equal.equalA[A]
  implicit def fallbackShow[A]: Show[A] = Show.showA[A]
  */
}


class GenericSymbolDeltaSpec extends FreeSpec with Matchers {
  import sjc.delta.Delta._
  import sjc.delta.std.int._
  import sjc.delta.generic.GenericSymbolDelta._

  import record._
  import ops.record._
  import syntax.singleton._

  "something" in {
    val gen = LabelledGeneric[HasInt]

    val keys = Keys[gen.Repr]

//    println(gen.to(HasInt(123)))

//    println(keys.apply())

//    println((HasInt(1): Thing) delta (HasInt(2): Thing))
  }

//  case object OtherThing extends Thing
}

sealed trait Thing
case class HasInt(i: Int) extends Thing
