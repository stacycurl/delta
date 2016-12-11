package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, JsonObject, PrettyParams}
import org.scalatest.matchers.{MatchResult, Matcher}
import sjc.delta.matchers.{DeltaMatcher, Pretty}
import sjc.delta.{Delta, Patch}


object matchers {
  def beDifferentTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): DeltaMatcher[A, Json] with matchers =
    new DeltaMatcher(expected, false) with matchers

  def beIdenticalTo[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[A, Json]): DeltaMatcher[A, Json] with matchers =
    new DeltaMatcher(expected, true) with matchers

  def beConsistentWith[A: EncodeJson](expected: A)(implicit deltaA: Delta.Aux[Json, Json]): DeltaConsistencyMatcher[A] with matchers =
    new DeltaConsistencyMatcher(expected, true) with matchers


  class DeltaConsistencyMatcher[A: EncodeJson](expected: A, positive: Boolean)(implicit deltaA: Delta.Aux[Json, Json])
    extends Matcher[A] {

    def apply(actual: A): MatchResult = {
      val expectedJ = EncodeJson.of[A].encode(expected)
      val actualJ   = EncodeJson.of[A].encode(actual)
      val maskedExpected = mask(actualJ, expectedJ)
      val maskedActual   = mask(expectedJ, actualJ)

      val delta = deltaA(maskedActual, maskedExpected)

      matchResult(delta, s"Detected the following inconsistencies:\n  ${Pretty[Json].indent(delta)}")
    }

    private def matchResult(delta: Json, positiveMsg: String) = {
      if (positive) MatchResult(Patch[Json].isEmpty(delta),  positiveMsg, "No inconsistencies detected")
      else          MatchResult(Patch[Json].nonEmpty(delta), "No inconsistencies detected", positiveMsg)
    }

    private def mask(mask: Json, toMask: Json): Json = {
      def recurse(maskJ: Json, toMaskJ: Json): Json = {
        (maskJ, toMaskJ) match {
          case (JObject(maskO), JObject(toMaskO)) ⇒ {
            Json.jObject(
              JsonObject.fromTraversableOnce(toMaskO.toMap.filterKeys(maskO.fieldSet))
            )
          }
          case (JArray(List(maskE)), JArray(toMaskA)) ⇒ {
            Json.jArray(toMaskA.map(toMaskE ⇒ recurse(maskE, toMaskE)))
          }
          case _ ⇒ toMaskJ
        }
      }

      recurse(mask, toMask)
    }

  }


  implicit class DeltaMatcherJsonOps[A](val value: DeltaMatcher[A, Json] with matchers) extends AnyVal {
    def ignoring(paths: String*): Matcher[A] = value.map(delta ⇒ delta.withObject(obj ⇒ paths.foldLeft(obj)(_ - _)))
  }

  implicit val prettyJson: Pretty[Json] =
    Pretty.create[Json](json ⇒ PrettyParams.spaces2.copy(preserveOrder = true).pretty(json))

  implicit val patchJson: Patch[Json] =
    Patch.create[Json](_ == Json.jEmptyObject)

  private object JObject { def unapply(json: Json): Option[JsonObject] = json.obj   }
  private object JArray  { def unapply(json: Json): Option[List[Json]] = json.array }
}

// This trait exists to help drive implicit lookup, I want users of 'beIdenticalTo' to be able to chain with calls
// on DeltaMatchJsonOps, but I don't want them to have to import the latter, so I need it to be discovered, it will be
// because DeltaMatchJsonOps is on the companion object to the return type of beIdenticalTo.
trait matchers