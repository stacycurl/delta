package sjc.delta

import org.junit.Assert

import scala.util.Try

object SpecyOps {
  implicit class SpecyOps[A](actual: A) {
    def shouldEqualBy(f: A ⇒ A, expected: A): Unit = // f(a) != f(b) ⇒ a != b, and the latter fails better
      Try(Assert.assertEquals(f(expected), f(actual))).failed.foreach(_ ⇒ shouldEqual(expected))

    def shouldEqual(expected: Any): Unit = Assert.assertEquals(expected, actual)
  }
}
