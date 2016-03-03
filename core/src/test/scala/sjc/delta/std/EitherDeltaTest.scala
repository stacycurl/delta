package sjc.delta.std

import org.junit.Test
import scalaz.{Show, Equal, \/}

import sjc.delta.Delta
import sjc.delta.Delta.DeltaOps
import sjc.delta.SpecyOps.SpecyOps
import sjc.delta.std.either.{deltaEither, EitherPatch, BothLeft, BothRight, WasLeft, WasRight}
import sjc.delta.std.int.deltaInt


class EitherDeltaTest {
  @Test def eitherDeltaTest(): Unit = {
    type E = Either[Int, Int]
    def left(l: Int): E = Left(l)
    def right(r: Int): E = Right(r)

    left(2).delta(left(10))   shouldEqual bothLeft(8)
    right(2).delta(right(10)) shouldEqual bothRight(8)
    left(2).delta(right(10))  shouldEqual wasLeft(2, 10)
    right(2).delta(left(10))  shouldEqual wasRight(2, 10)
  }

  @Test def \\/(): Unit = {
    implicit def deltaV[L, R](implicit deltaEither: Delta[Either[L, R]]): Delta[L \/ R] =
      deltaEither.contramap[L \/ R](_.toEither)

    type E = \/[Int, Int]
    def left(l: Int): E = \/.left[Int, Int](l)
    def right(r: Int): E = \/.right[Int, Int](r)

    left(2).delta(left(10))   shouldEqual bothLeft(8)
    right(2).delta(right(10)) shouldEqual bothRight(8)
    left(2).delta(right(10))  shouldEqual wasLeft(2, 10)
    right(2).delta(left(10))  shouldEqual wasRight(2, 10)
  }

  private type EP = EitherPatch[Int, Int, Int, Int]

  private def bothLeft(out: Int): EP = BothLeft[Int](out)
  private def bothRight(out: Int): EP = BothRight[Int](out)
  private def wasLeft(l: Int, r: Int): EP = WasLeft[Int, Int](l, r)
  private def wasRight(r: Int, l: Int): EP = WasRight[Int, Int](r, l)

  private implicit def eitherPatchEqual[L, R, LOut, ROut](
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

  private implicit def eitherPatchShow[L, R, LOut, ROut](
    implicit lshow: Show[L], rshow: Show[R], loutShow: Show[LOut], routShow: Show[ROut]
  ): Show[EitherPatch[L, R, LOut, ROut]] = new Show[EitherPatch[L, R, LOut, ROut]] {
    override def shows(in: EitherPatch[L, R, LOut, ROut]): String = in match {
      case BothLeft(out)         => s"BothLeft(${loutShow.show(out)})"
      case BothRight(out)        => s"BothRight(${routShow.show(out)})"
      case WasLeft(left, right)  => s"WasLeft(${lshow.show(left)}, ${rshow.show(right)})"
      case WasRight(right, left) => s"WasRight(${rshow.show(right)}, S{lshow.show(left)})"
    }
  }
}
