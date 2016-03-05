package sjc.delta

import org.junit.Assert

import scala.util.Try


trait TestUtil {
  implicit class AnyTestOps[A](actual: A) {
    def shouldEqualBy[B](f: A ⇒ B, expected: A): Unit = // f(a) != f(b) ⇒ a != b, and the latter fails better
      Try(Assert.assertEquals(f(expected), f(actual))).failed.foreach(_ ⇒ shouldEqual(expected))

    def shouldEqual(expected: Any): Unit = Assert.assertEquals(expected, actual)
  }
}