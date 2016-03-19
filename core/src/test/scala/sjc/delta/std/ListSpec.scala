package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.std.int.deltaInt
import sjc.delta.std.list.{deltaList, Diff, Extra, Missing}
import sjc.delta.Delta.DeltaOps


class ListSpec extends FreeSpec with Matchers {
  "list" in {
    List(1, 2, 3, 4) delta List(1, 0, 6) shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Missing(3, 4))
    List(1, 2, 3) delta List(1, 0, 6, 9) shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Extra(3, 9))
  }
}
