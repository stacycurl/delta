package sjc.delta.std

import org.junit.Test
import scalaz.{Show, Equal, \/}

import sjc.delta.{TestUtil, Delta}
import sjc.delta.Delta.DeltaOps
import sjc.delta.std.either.{deltaEither, EitherPatch, BothLeft, BothRight, WasLeft, WasRight}
import sjc.delta.std.int.deltaInt


class EitherTest extends TestUtil {
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
    // TODO: Create a 'scalaz' module and move this definition there
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

    def equal(left: EP, right: EP): Boolean = (left, right) match {
      case (BothLeft(blLeft), BothLeft(blRight)) => {
        loutEqual.equal(blLeft, blRight)
      }
      case (BothRight(brLeft), BothRight(brRight)) => {
        routEqual.equal(brLeft, brRight)
      }
      case (WasLeft(lLeft, rLeft), WasLeft(lRight, rRight)) => {
        lEqual.equal(lLeft, lRight) && rEqual.equal(rLeft, rRight)
      }
      case (WasRight(rLeft, lLeft), WasRight(rRight, lRight)) => {
        rEqual.equal(rLeft, rRight) && lEqual.equal(lLeft, lRight)
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
