package sjc.delta.argonaut

import argonaut.{Json, JsonObject}
import sjc.delta.Delta


object json {
  implicit val jsonDelta: Delta.Aux[Json, List[String]] =
    Delta.from[Json].curried[List[String]](actualJ => expectedJ => deltas(actualJ, expectedJ))

  def deltas(actualJ: Json, expectedJ: Json): List[String] = {
    def recurse(context: Context, actual: Option[Json], expected: Option[Json]): List[String] = {
      if (actual == expected) Nil else (JSON.get(actual), JSON.get(expected)) match {
        case (Some(ObjectJSON(actualO)), Some(ObjectJSON(expectedO))) ⇒
          val fields = (actualO.fieldSet ++ expectedO.fieldSet).toList

          fields.flatMap(field ⇒ {
            recurse(context + field, actualO.apply(field), expectedO.apply(field))
          })

        case (Some(ArrayJSON(actualA)), Some(ArrayJSON(expectedA))) ⇒
          Range(0, actualA.length max expectedA.length).toList.flatMap(index ⇒ {
            recurse(context + index.toString, actualA.lift(index), expectedA.lift(index))
          })

        case _ ⇒ context.error(expected, actual)
      }
    }

    recurse(Context(Nil), Some(actualJ), Some(expectedJ))
  }

  private object JSON {
    def get(oj: Option[Json]): Option[JSON] = oj.flatMap(_.fold[Option[JSON]](
      jsonNull   = None,
      jsonBool   = _ => None,
      jsonNumber = _ => None,
      jsonString = _ => None,
      jsonArray  = arr ⇒ Some(ArrayJSON(arr)),
      jsonObject = obj ⇒ Some(ObjectJSON(obj))
    ))
  }

  private sealed trait JSON // types of JSON that we need to recurse into
  private case class ObjectJSON(value: JsonObject) extends JSON
  private case class ArrayJSON(value: List[Json]) extends JSON

  private case class Context(elements: List[String]) {
    def +(element: String): Context = copy(element :: elements)

    def error(expectedOJ: Option[Json], actualOJ: Option[Json]): List[String] = {
      val expectedS = expectedOJ.fold("nothing")(_.spaces2)
      val actualS   = actualOJ.fold("missing")(_.spaces2)

      List(s"$prefix\n\texpected:\n${indent(expectedS)}\n\tbut was:\n${indent(actualS)}")
    }

    private def prefix: String = if (elements.isEmpty) "" else elements.reverse.mkString("/") + ":"
    private def indent(in: String): String = in.split("\n").toList.map("\t\t" + _).mkString("\n")
  }
}