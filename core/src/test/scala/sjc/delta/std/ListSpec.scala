package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.std.int.deltaInt
import sjc.delta.Delta.DeltaOps
import sjc.delta.std.list.patience._


class ListSpec extends FreeSpec with Matchers {
  "naive" in {
    import sjc.delta.std.list.naive.{deltaList, Diff, Extra, Missing}

    List(1, 2, 3, 4) delta List(1, 0, 6) shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Missing(3, 4))
    List(1, 2, 3) delta List(1, 0, 6, 9) shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Extra(3, 9))
  }

  "patience" in {
    val delta = sjc.delta.std.list.patience.deltaList[Int]

    delta(List(0, 2, 3, 4), List(2, 3, 4)) shouldBe List(
      Removed(SubSeq(0, 0, 1, 0), List(0)),
      Equal(SubSeq(1, 0, 3, 3), List(2, 3, 4))
    )

    delta(List(2, 3, 4), List(1, 2, 3, 4)) shouldBe List(
      Inserted(SubSeq(0, 0, 0, 1), List(1)),
      Equal(SubSeq(0, 1, 3, 3), List(2, 3, 4))
    )

    delta(List(0, 2, 3, 4), List(1, 2, 3, 4)) shouldBe List(
      Replaced(SubSeq(0, 0, 1, 1), List(0), List(1)),
      Equal(SubSeq(1, 1, 3, 3), List(2, 3, 4))
    )

    delta(List(0, 2, 3, 4, 8), List(1, 2, 3, 4, 9)) shouldBe List(
      Replaced(SubSeq(0, 0, 1, 1), List(0), List(1)),
      Equal(SubSeq(1, 1, 3, 3), List(2, 3, 4)),
      Replaced(SubSeq(4, 4, 1, 1), List(8), List(9))
    )

    val left  = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1000, 1001, 1002, 1003, 2000)
    val right = List(1, 2, 3, 4, 77, 5, 6, 9, 100, 111, 1000, 2000, 2004, 2005, 2006)

    delta(left, right) shouldBe List(
      Equal(SubSeq(0, 0, 4, 4),     List(1, 2, 3, 4)),
      Inserted(SubSeq(0, 4, 0, 1),  List(77)),
      Equal(SubSeq(4, 5, 2, 2),     List(5, 6)),
      Removed(SubSeq(6, 0, 2, 0),   List(7, 8)),
      Equal(SubSeq(8, 7, 1, 1),     List(9)),
      Replaced(SubSeq(9, 8, 2, 2),  List(10, 11), List(100, 111)),
      Equal(SubSeq(11, 10, 1, 1),   List(1000)),
      Removed(SubSeq(12, 0, 3, 0),  List(1001, 1002, 1003)),
      Equal(SubSeq(15, 11, 1, 1),   List(2000)),
      Inserted(SubSeq(0, 12, 0, 3), List(2004, 2005, 2006))
    )
  }
}
