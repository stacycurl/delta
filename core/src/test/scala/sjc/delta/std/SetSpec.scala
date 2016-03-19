package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.Delta.DeltaOps
import sjc.delta.std.set.{deltaSet, SetPatch}


class SetSpec extends FreeSpec with Matchers {
  "set" in {
    Set(1, 2).delta(Set(2, 3)) shouldBe SetPatch(removed = Set(1), added = Set(3))
  }
}