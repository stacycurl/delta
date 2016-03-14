package sjc.delta.scalaz

import org.scalatest.{FreeSpec, Matchers}
import sjc.delta.std.either._

import scalaz.\/
import sjc.delta.Delta.DeltaOps
import sjc.delta.scalaz.disjunction.deltaV
import sjc.delta.std.int.deltaInt


class DisjunctionSpec extends FreeSpec with Matchers {
  "\\/" in {
    type E = \/[Int, Int]
    def left(l: Int): E = \/.left[Int, Int](l)
    def right(r: Int): E = \/.right[Int, Int](r)

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
