package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, JsonObject}
import argonaut.Json.jEmptyObject
import sjc.delta.Delta

import scala.util.Try


object json {
  object flat {
    implicit val jsonDelta: Delta.Aux[Json, Json] =
      Delta.from[Json].curried[Json](actualJ => expectedJ => deltas(actualJ, expectedJ))
  }

  object compressed {
    private implicit class JsonOps(val json: Json) extends AnyVal {
      def add(path: List[String], value: Json): Json = path match {
        case Nil => json
        case last :: Nil => json.withObject(_ + (last, value))
        case head :: tail => json.obj.fold(json)(obj => {
          Json.jObject(obj + (head, obj(head).getOrElse(jEmptyObject).add(tail, value)))
        })
      }
    }

    implicit val jsonDelta: Delta.Aux[Json, Json] = json.flat.jsonDelta.map(json => {
      json.obj.fold(json)(obj => {
        obj.toMap.foldLeft(jEmptyObject) {
          case (acc, (context, delta)) => acc.add(context.split("/").toList, delta)
        }
      })
    })
  }

  object generic {
    object flat {
      implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] =
        json.flat.jsonDelta.contramap[A](EncodeJson.of[A].encode)
    }

    object compressed {
      implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] =
        json.compressed.jsonDelta.contramap[A](EncodeJson.of[A].encode)
    }
  }

  def deltas(actualJ: Json, expectedJ: Json): Json = {
    def recurse(context: Context, actual: Option[Json], expected: Option[Json]): List[(String, Json)] = {
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

    Json.jObjectFields(recurse(Context(Nil), Some(actualJ), Some(expectedJ)): _*)
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

    def error(expectedOJ: Option[Json], actualOJ: Option[Json]): List[(String, Json)] = List(
      prefix -> (actualOJ.map("actual" -> _) ->?: expectedOJ.map("expected" -> _) ->?: jEmptyObject)
    )

    private def prefix: String = if (elements.isEmpty) "" else elements.reverse.mkString("/")
  }
}