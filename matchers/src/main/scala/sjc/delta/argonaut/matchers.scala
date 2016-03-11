package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, PrettyParams}
import org.scalatest.matchers.Matcher
import sjc.delta.DeltaWithZero
import sjc.delta.matchers.{DeltaMatcher, Pretty}


object matchers {
  def beDifferentTo[A: EncodeJson](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, Json]): DeltaMatcher[A, Json] =
    new DeltaMatcher(expected, false)

  def beIdenticalTo[A: EncodeJson](expected: A)(implicit deltaA: DeltaWithZero.Aux[A, Json]): Matcher[A] =
    new DeltaMatcher(expected, true)

  implicit val prettyJson: Pretty[Json] =
    Pretty.create[Json](json ⇒ PrettyParams.spaces2.copy(preserveOrder = true).pretty(json))
}