package sjc.delta.std

import org.junit.Test
import sjc.delta.Delta.DeltaOps
import sjc.delta.TestUtil

import sjc.delta.std.int.deltaInt
import sjc.delta.std.map.{deltaMap, MapPatch}


class MapTest extends TestUtil {
  @Test def simpleMap(): Unit = {
    leftM delta rightM  shouldEqual expectedM
  }

  @Test def nestedMaps(): Unit = {
    val nested = Map("a" -> Map(1 -> 1), "b" -> leftM).delta(Map("b" -> rightM, "c" -> Map(3 -> 3)))

    nested shouldEqual MapPatch(
      added   = Map("c" -> Map(3 -> 3)),
      removed = Map("a" -> Map(1 -> 1)),
      changed = Map("b" -> expectedM)
    )
  }

  private val leftM = Map(1 -> 1, 2 -> 2)
  private val rightM  = Map(2 -> 22, 3 -> 3)

  private val expectedM = MapPatch(
    added   = Map(3 -> 3),
    removed = Map(1 -> 1),
    changed = Map(2 -> (2 delta 22))
  )
}