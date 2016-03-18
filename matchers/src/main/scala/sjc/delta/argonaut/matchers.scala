package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, PrettyParams}
import org.scalatest.matchers.Matcher
import sjc.delta.{Patch, Delta}
import sjc.delta.matchers.{DeltaMatcher, Pretty}


object matchers {
  def beDifferentTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): DeltaMatcher[A, Json] with matchers =
    new DeltaMatcher(expected, false) with matchers

  def beIdenticalTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): DeltaMatcher[A, Json] with matchers =
    new DeltaMatcher(expected, true) with matchers

  implicit class DeltaMatcherJsonOps[A](val value: DeltaMatcher[A, Json] with matchers) extends AnyVal {
    def ignoring(paths: String*): Matcher[A] = value.map(delta ⇒ delta.withObject(obj ⇒ paths.foldLeft(obj)(_ - _)))
  }

  implicit val prettyJson: Pretty[Json] =
    Pretty.create[Json](json ⇒ PrettyParams.spaces2.copy(preserveOrder = true).pretty(json))

  implicit val patchJson: Patch[Json] =
    Patch.create[Json](_ == Json.jEmptyObject)
}

// This trait exists to help drive implicit lookup, I want users of 'beIdenticalTo' to be able to chain with calls
// on DeltaMatchJsonOps, but I don't want them to have to import the latter, so I need it to be discovered, it will be
// because DeltaMatchJsonOps is on the companion object to the return type of beIdenticalTo.
trait matchers