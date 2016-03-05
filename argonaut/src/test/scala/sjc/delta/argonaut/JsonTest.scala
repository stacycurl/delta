package sjc.delta.argonaut

import _root_.argonaut.Json
import _root_.argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.junit.Test
import sjc.delta.TestUtil
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.jsonDelta


class JsonTest extends TestUtil {
  @Test def shouldIgnoreIdenticalElements(): Unit = {
    jNull          delta jNull          shouldEqual Nil
    jBool(true)    delta jBool(true)    shouldEqual Nil
    jNumber(123)   delta jNumber(123)   shouldEqual Nil
    jString("abc") delta jString("abc") shouldEqual Nil
    jEmptyObject   delta jEmptyObject   shouldEqual Nil
    jArray(Nil)    delta jArray(Nil)    shouldEqual Nil

    jArray(List(jString("abc")))              delta jArray(List(jString("abc")))              shouldEqual Nil
    ("foo" → jString("abc")) ->: jEmptyObject delta ("foo" → jString("abc")) ->: jEmptyObject shouldEqual Nil

    ("foo" → jString("abc")) ->: ("bar" → jString("abc")) ->: jEmptyObject delta
    ("bar" → jString("abc")) ->: ("foo" → jString("abc")) ->: jEmptyObject shouldEqual Nil
  }

  @Test def shouldListDifferentElements(): Unit =  {
    jString("abc") delta jString("def") shouldEqual List(
      expectedMessage(context = "", expected = Some(jString("def")), actual = Some(jString("abc")))
    )

    ("foo" → jString("abc")) ->: jEmptyObject delta ("foo" → jString("def")) ->: jEmptyObject shouldEqual List(
      expectedMessage(context = "foo:", expected = Some(jString("def")), actual = Some(jString("abc")))
    )

    jArray(List(jString("abc"))) delta jArray(List(jString("def"))) shouldEqual List(
      expectedMessage(context = "0:", expected = Some(jString("def")), actual = Some(jString("abc")))
    )
  }

  @Test def shouldListMissingElements(): Unit =  {
    jEmptyObject delta ("parent" → jString("def")) ->: jEmptyObject shouldEqual List(
      expectedMessage(context = "parent:", expected = Some(jString("def")), actual = None)
    )

    jArray(Nil) delta jArray(List(jString("def"))) shouldEqual List(
      expectedMessage(context = "0:", expected = Some(jString("def")), actual = None)
    )
  }

  @Test def shouldListExtraElements(): Unit = {
    json.deltas(("parent" → jString("def")) ->: jEmptyObject, jEmptyObject) shouldEqual List(
      expectedMessage(context = "parent:", expected = None, actual = Some(jString("def")))
    )

    jArray(List(jString("def"))) delta jArray(Nil) shouldEqual List(
      expectedMessage(context = "0:", expected = None, actual = Some(jString("def")))
    )
  }

  private def expectedMessage(context: String, expected: Option[Json], actual: Option[Json]): String = {
    val expectedS = expected.fold("nothing")(_.spaces2)
    val actualS   = actual.fold("missing")(_.spaces2)

    s"$context\n\texpected:\n\t\t$expectedS\n\tbut was:\n\t\t$actualS"
  }
}
