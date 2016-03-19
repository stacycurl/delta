package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.Delta.DeltaOps
import sjc.delta.std.set.{deltaSet, SetPatch}


class SetSpec extends FreeSpec with Matchers {
  "set" in {
    val expected = SetPatch(removed = Set(1), added = Set(3))

    Set(1, 2).delta(Set(2, 3)) shouldBe expected
  }
}