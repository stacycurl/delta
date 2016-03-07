package sjc.delta.argonaut

import argonaut.{Json, Parse, PrettyParams}
import sjc.delta.TestUtil


trait JsonTestUtil extends TestUtil {
  implicit class JsonTestOps(json: Json) {
    def jsonShouldEqual(expected: String): Unit = json jsonShouldEqual parse(expected)
    def jsonShouldEqual(expected: Json): Unit = preserveOrder.pretty(json) shouldEqual preserveOrder.pretty(expected)
  }

  def parse(content: String): Json = Parse.parseOption(content).getOrElse(sys.error("not json"))

  private val preserveOrder = PrettyParams.spaces2.copy(preserveOrder = true)
}