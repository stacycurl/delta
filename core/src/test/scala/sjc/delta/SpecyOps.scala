package sjc.delta

import org.junit.Assert

object SpecyOps {
  implicit class SpecyOps[A](actual: A) {
    def shouldEqual(expected: Any): Unit = Assert.assertEquals(expected, actual)
  }
}
