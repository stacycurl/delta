package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}

import sjc.delta.Delta.DeltaOps
import sjc.delta.std.either.{deltaEither, EitherPatch, BothLeft, BothRight, WasLeft, WasRight}
import sjc.delta.std.int.deltaInt


class EitherSpec extends FreeSpec with Matchers {
  "either" in {
    type E = Either[Int, Int]
    def left(l: Int): E = Left(l)
    def right(r: Int): E = Right(r)

    left(2).delta(left(10))   shouldBe bothLeft(8)
    right(2).delta(right(10)) shouldBe bothRight(8)
    left(2).delta(right(10))  shouldBe wasLeft(2, 10)
    right(2).delta(left(10))  shouldBe wasRight(2, 10)
  }

  private type EP = EitherPatch[Int, Int, Int, Int]

  private def bothLeft(out: Int): EP = BothLeft[Int](out)
  private def bothRight(out: Int): EP = BothRight[Int](out)
  private def wasLeft(l: Int, r: Int): EP = WasLeft[Int, Int](l, r)
  private def wasRight(r: Int, l: Int): EP = WasRight[Int, Int](r, l)
}
