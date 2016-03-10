package sjc.delta.argonaut

import argonaut.{PrettyParams, EncodeJson, Json}
import org.scalatest.matchers.Matcher
import sjc.delta.Delta
import sjc.delta.matchers.DeltaMatcher


object matchers {
  def beDifferentTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): DeltaMatcher[A, Json] =
    new DeltaMatcher(expected, Json.jEmptyObject, None, pretty, false)

  def beIdenticalTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): Matcher[A] =
    new DeltaMatcher(expected, Json.jEmptyObject, None, pretty, true)

  private def pretty(json: Json): String = PrettyParams.spaces2.copy(preserveOrder = true).pretty(json)
}