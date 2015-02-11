package sjc.delta

import org.specs2.execute.{Success, Failure, Result, AsResult}
import org.specs2.matcher.Matcher
import org.specs2.scalaz.{Spec, ScalazMatchers}
import scalaz.{Equal, Lens, Show, \/}
import shapeless._

import scalaz.std.either._


class DeltaTest extends Spec with ScalazMatchers {
  import scalaz.std.AllInstances._
  import sjc.delta.Delta._

  "int delta" should {
    "return an int" in {
      import sjc.delta.Delta.std.int._

      10.delta(2) must equal(-8)
    }
  }

  "either delta" in {
    import Delta.std.int._
    import Delta.std.either._

    type E  = Either[Int, Int]
    type EP = EitherPatch[Int, Int, Int, Int]

    def left(l: Int): E = Left(l)
    def right(r: Int): E = Right(r)
    def bothLeft(out: Int): EP = BothLeft[Int](out)
    def bothRight(out: Int): EP = BothRight[Int](out)
    def wasLeft(l: Int, r: Int): EP = WasLeft[Int, Int](l, r)
    def wasRight(r: Int, l: Int): EP = WasRight[Int, Int](r, l)

    left(2).delta(left(10))   must equal(bothLeft(8))
    right(2).delta(right(10)) must equal(bothRight(8))
    left(2).delta(right(10))  must equal(wasLeft(2, 10))
    right(2).delta(left(10))  must equal(wasRight(2, 10))
  }

  "\\/ delta" in {
    import sjc.delta.Delta.std.int._
    import sjc.delta.Delta.std.either._

    implicit def deltaV[L, R](implicit deltaEither: Delta[Either[L, R]]): Delta[L \/ R] =
      deltaEither.contramap[L \/ R](_.toEither)

    type E  = \/[Int, Int]
    type EP = EitherPatch[Int, Int, Int, Int]

    def left(l: Int): E = \/.left[Int, Int](l)
    def right(r: Int): E = \/.right[Int, Int](r)
    def bothLeft(out: Int): EP = BothLeft[Int](out)
    def bothRight(out: Int): EP = BothRight[Int](out)
    def wasLeft(l: Int, r: Int): EP = WasLeft[Int, Int](l, r)
    def wasRight(r: Int, l: Int): EP = WasRight[Int, Int](r, l)

    left(2).delta(left(10))   must equal(bothLeft(8))
    right(2).delta(right(10)) must equal(bothRight(8))
    left(2).delta(right(10))  must equal(wasLeft(2, 10))
    right(2).delta(left(10))  must equal(wasRight(2, 10))
  }

  "set delta" in {
    import sjc.delta.Delta.std.set._

    val expected = SetPatch(removed = Set(1), added = Set(3))

    Set(1, 2).delta(Set(2, 3)) must equal(expected)
  }

  "map delta" in {
    import sjc.delta.Delta.std.int._
    import sjc.delta.Delta.std.map._

    beforeM.delta(afterM) must equal(expectedM)

    val nested = Map("a" -> Map(1 -> 1), "b" -> beforeM).delta(Map("b" -> afterM, "c" -> Map(3 -> 3)))

    nested must equal(MapPatch(
      added   = Map("c" -> Map(3 -> 3)),
      removed = Map("a" -> Map(1 -> 1)),
      changed = Map("b" -> expectedM)
    ))
  }

  "hlist delta" in {
    import sjc.delta.Delta.std.int._

    (1 :: 10 :: HNil).delta(3 :: 30 :: HNil) must equal(1.delta(3) :: 10.delta(30) :: HNil)

    (1 :: 10 :: HNil).zipWith(3 :: 30 :: HNil)(deltaPoly) must equal(1.delta(3) :: 10.delta(30) :: HNil)
  }

  "coproduct delta" in {
    import sjc.delta.Delta.std.int._

    type CPatch[H, T <: Coproduct] = H :+: (H, T) :+: (T, H) :+: CNil

    type E  = Int :+: Int :+: Int :+: CNil
    type EP = CPatch[Int, Int :+: Int :+: CNil] :+: CPatch[Int, Int :+: CNil] :+: CPatch[Int, CNil] :+: CNil

    (Inl(2): E).delta(Inl(10): E)           must equal(Inl(Inl(8)): EP)
    (Inr(Inl(10)): E).delta(Inr(Inl(2)): E) must equal(Inr(Inl(Inl((-8)))): EP)
    (Inl(2): E).delta(Inr(Inl(10)): E)      must equal(Inl(Inr(Inl((2, Inl(10))))): EP)
    (Inr(Inl(2)): E).delta(Inl(10): E)      must equal(Inl(Inr(Inr(Inl((Inl(2), 10))))): EP)
  }

  "create delta from function" in {
    implicit val doubleDelta = Delta.from[Double] { case (before, after) => after - before }

    1.5.delta(2.0) must equal(0.5)

    implicit val stringDelta = Delta.from[String].curried(before => after => (before, after))

    "foo".delta("bar") must equal(("foo", "bar"))
  }

  "can map over delta" in {
    implicit val intDeltaAsString: Delta.Aux[Int, String] = Delta.std.int.deltaInt.map(_.toString)

    1.delta(3) must equal("2")
  }

  "can contramap over delta" in {
    import sjc.delta.Delta.std.int._

    implicit val hasIntDelta = Delta[Int].contramap[HasInt](_.i)

    HasInt(1).delta(HasInt(2)) must equal(1.delta(2))
  }

  "fallback delta" in {
    import Delta.fallback._

    HasInt(1).delta(HasInt(2)) must equal((HasInt(1), HasInt(2)))
  }

  "generic delta" in {
    import sjc.delta.Delta.std.int._
    import sjc.delta.Delta.std.map._

    HasInt(1).delta(HasInt(2)) must equal(1.delta(2) :: HNil)
    MapAndInt(1, beforeM).delta(MapAndInt(2, afterM)) must equal(1.delta(2) :: expectedM :: HNil)

    val actual = RecursiveProduct(1, None).delta(RecursiveProduct(3, None))

    val expected: ::[Aux[Int, Int]#Out, ::[Inl[Inl[HNil.type, Nothing], Nothing], HNil]] = 1.delta(3) :: Inl(Inl(HNil)) :: HNil

    actual must equal(expected)

    // Doesn't work yet
//    RecursiveProduct(1, Some(RecursiveProduct(10, None))).delta(RecursiveProduct(3, Some(RecursiveProduct(30, None))))
//      .must(equal(1.delta(3) :: Inl(Inl(10.delta(30)) :: Inl(Inl(HNil)) :: HNil) :: HNil))
  }


  "function delta" in {
    import sjc.delta.Delta.std.int._
    import sjc.delta.Delta.function._

    val square = (i: Int) => i * i
    val cube = (i: Int) => i * i * i

    val delta = square.delta(cube)

    delta(3) must equal(27 - 9)
  }

  case class HasInt(i: Int)
  case class MapAndInt(i: Int, m: Map[Int, Int])
  case class RecursiveProduct(i: Int, o: Option[RecursiveProduct])

  val beforeM = Map(1 -> 1, 2 -> 2)
  val afterM  = Map(2 -> 22, 3 -> 3)

  val expectedM = Delta.std.map.MapPatch(
    added   = Map(3 -> 3),
    removed = Map(1 -> 1),
    changed = Map(2 -> Delta.std.int.deltaInt(2, 22))
  )

  import sjc.delta.Delta.std.either._

  implicit def eitherPatchEqual[L, R, LOut, ROut](
    implicit lEqual: Equal[L], rEqual: Equal[R], loutEqual: Equal[LOut], routEqual: Equal[ROut]
  ): Equal[EitherPatch[L, R, LOut, ROut]] = new Equal[EitherPatch[L, R, LOut, ROut]] {
    type EP = EitherPatch[L, R, LOut, ROut]

    def equal(before: EP, after: EP): Boolean = (before, after) match {
      case (BothLeft(blBefore), BothLeft(blAfter)) => {
        loutEqual.equal(blBefore, blAfter)
      }
      case (BothRight(brBefore), BothRight(brAfter)) => {
        routEqual.equal(brBefore, brAfter)
      }
      case (WasLeft(lBefore, rBefore), WasLeft(lAfter, rAfter)) => {
        lEqual.equal(lBefore, lAfter) && rEqual.equal(rBefore, rAfter)
      }
      case (WasRight(rBefore, lBefore), WasRight(rAfter, lAfter)) => {
        rEqual.equal(rBefore, rAfter) && lEqual.equal(lBefore, lAfter)
      }
      case _ => false
    }
  }

  implicit def eitherPatchShow[L, R, LOut, ROut](
    implicit lshow: Show[L], rshow: Show[R], loutShow: Show[LOut], routShow: Show[ROut]
  ): Show[EitherPatch[L, R, LOut, ROut]] = new Show[EitherPatch[L, R, LOut, ROut]] {
    override def shows(in: EitherPatch[L, R, LOut, ROut]): String = in match {
      case BothLeft(out)         => s"BothLeft(${loutShow.show(out)})"
      case BothRight(out)        => s"BothRight(${routShow.show(out)})"
      case WasLeft(left, right)  => s"WasLeft(${lshow.show(left)}, ${rshow.show(right)})"
      case WasRight(right, left) => s"WasRight(${rshow.show(right)}, S{lshow.show(left)})"
    }
  }

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

// vim: expandtab:ts=2:sw=2
