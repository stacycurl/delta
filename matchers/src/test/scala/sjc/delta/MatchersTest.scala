package sjc.delta

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.matchers.{beDifferentTo, beIdenticalTo}
import sjc.delta.std.int.deltaInt


class MatchersTest extends FreeSpec with Matchers {
  "beDifferentTo" in {
    1 should beDifferentTo(20)

    intercept[TestFailedException] {
      1 should beDifferentTo(1)
    }.message shouldBe Some("No differences detected")
  }

  "beDifferentTo.withDelta" in {
    1 should beDifferentTo(20).withDelta(19)

    intercept[TestFailedException] {
      1 should beDifferentTo(20).withDelta(5)
    }.message shouldBe Some("Difference was not as expected\n  actual: 19\n  expected: 5")
  }

  "beIdenticalTo" in {
    1 should beIdenticalTo(1)

    intercept[TestFailedException] {
      1 should beIdenticalTo(20)
    }.message shouldBe Some("Detected the following differences:\n  19")
  }
}
