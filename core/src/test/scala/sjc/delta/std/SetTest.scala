package sjc.delta.std

import sjc.delta.Delta.DeltaOps
import sjc.delta.std.set.{deltaSet, SetPatch}

import org.junit.Test
import sjc.delta.TestUtil


class SetTest extends TestUtil {
  @Test def setDeltaTest(): Unit ={
    val expected = SetPatch(removed = Set(1), added = Set(3))

    Set(1, 2).delta(Set(2, 3)) shouldEqual expected
  }
}