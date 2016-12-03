package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import sjc.delta.std.int.deltaInt
import sjc.delta.Delta.DeltaOps
import sjc.delta.std.list.patience._


class ListSpec extends FreeSpec with Matchers {
  "naive" in {
    import sjc.delta.std.list.naive.{deltaList, Diff, Extra, Missing}

    List(1, 2, 3, 4) delta List(1, 0, 6)    shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Missing(3, 4))
    List(1, 2, 3)    delta List(1, 0, 6, 9) shouldBe List(Diff(1, 2 delta 0), Diff(2, 3 delta 6), Extra(3, 9))
  }

  "patience" in {
    val delta = sjc.delta.std.list.patience.deltaList[Int]

    delta(List(0, 2, 3, 4), List(2, 3, 4)) shouldBe List(
      Removed(SubSeq(Span(0, 1), Span(0, 0)), List(0)),
      Equal(SubSeq(Span(1, 3), Span(0, 3)), List(2, 3, 4))
    )

    delta(List(2, 3, 4), List(1, 2, 3, 4)) shouldBe List(
      Inserted(SubSeq(Span(0, 0), Span(0, 1)), List(1)),
      Equal(SubSeq(Span(0, 3), Span(1, 3)), List(2, 3, 4))
    )

    delta(List(0, 2, 3, 4), List(1, 2, 3, 4)) shouldBe List(
      Replaced(SubSeq(Span(0, 1), Span(0, 1)), List(0), List(1)),
      Equal(SubSeq(Span(1, 3), Span(1, 3)), List(2, 3, 4))
    )

    delta(List(0, 2, 3, 4, 8), List(1, 2, 3, 4, 9)) shouldBe List(
      Replaced(SubSeq(Span(0, 1), Span(0, 1)), List(0), List(1)),
      Equal(SubSeq(Span(1, 3), Span(1, 3)),    List(2, 3, 4)),
      Replaced(SubSeq(Span(4, 1), Span(4, 1)), List(8), List(9))
    )

    val left  = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1000, 1001, 1002, 1003, 2000)
    val right = List(1, 2, 3, 4, 77, 5, 6, 9, 100, 111, 1000, 2000, 2004, 2005, 2006)

    delta(left, right) shouldBe List(
      Equal(SubSeq(Span(0, 4), Span(0, 4)),     List(1, 2, 3, 4)),
      Inserted(SubSeq(Span(0, 0), Span(4, 1)),  List(77)),
      Equal(SubSeq(Span(4, 2), Span(5, 2)),     List(5, 6)),
      Removed(SubSeq(Span(6, 2), Span(0, 0)),   List(7, 8)),
      Equal(SubSeq(Span(8, 1), Span(7, 1)),     List(9)),
      Replaced(SubSeq(Span(9, 2), Span(8, 2)),  List(10, 11), List(100, 111)),
      Equal(SubSeq(Span(11, 1), Span(10, 1)),   List(1000)),
      Removed(SubSeq(Span(12, 3), Span(0, 0)),  List(1001, 1002, 1003)),
      Equal(SubSeq(Span(15, 1), Span(11, 1)),   List(2000)),
      Inserted(SubSeq(Span(0, 0), Span(12, 3)), List(2004, 2005, 2006))
    )
  }
}
