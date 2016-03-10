package sjc.delta.argonaut

import argonaut.{PrettyParams, EncodeJson, Json}
import org.scalatest.matchers.Matcher
import sjc.delta.argonaut.json.actualExpected.flat.{jsonDelta, encodeJsonToDelta}
import sjc.delta.matchers.DeltaMatcher


object matchers {
  def beDifferentTo[A: EncodeJson](expected: A): DeltaMatcher[A, Json] =
    new DeltaMatcher(expected, Json.jEmptyObject, None, pretty, false)

  def beIdenticalTo[A: EncodeJson](expected: A): Matcher[A] =
    new DeltaMatcher(expected, Json.jEmptyObject, None, pretty, true)

  private def pretty(json: Json): String = PrettyParams.spaces2.copy(preserveOrder = true).pretty(json)
}