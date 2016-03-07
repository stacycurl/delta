package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, JsonObject}
import argonaut.Json.jEmptyObject
import sjc.delta.Delta

object json extends json("left", "right") {
  object beforeAfter    extends json("before", "after")
  object actualExpected extends json("actual", "expected")
}

case class json(lhsName: String, rhsName: String) { json =>
  object flat {
    implicit val jsonDelta: Delta.Aux[Json, Json] =
      Delta.from[Json].curried[Json](left => right => deltas(left, right))
  }

  object compressed {
    import util.JsonOps

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

  def deltas(leftJ: Json, rightJ: Json): Json = {
    def recurse(context: Context, left: Option[Json], right: Option[Json]): List[(String, Json)] = {
      if (left == right) Nil else (JSON.get(left), JSON.get(right)) match {
        case (Some(ObjectJSON(leftO)), Some(ObjectJSON(rightO))) ⇒
          val fields = (leftO.fieldSet ++ rightO.fieldSet).toList

          fields.flatMap(field ⇒ {
            recurse(context + field, leftO.apply(field), rightO.apply(field))
          })

        case (Some(ArrayJSON(leftA)), Some(ArrayJSON(rightA))) ⇒
          Range(0, leftA.length max rightA.length).toList.flatMap(index ⇒ {
            recurse(context + index.toString, leftA.lift(index), rightA.lift(index))
          })

        case _ ⇒ context.error(left, right)
      }
    }

    Json.jObjectFields(recurse(Context(Nil), Some(leftJ), Some(rightJ)): _*)
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

    def error(leftOJ: Option[Json], rightOJ: Option[Json]): List[(String, Json)] = List(
      prefix -> (leftOJ.map(lhsName -> _) ->?: rightOJ.map(rhsName -> _) ->?: jEmptyObject)
    )

    private def prefix: String = if (elements.isEmpty) "" else elements.reverse.mkString("/")
  }
}

private[argonaut] object util {
  implicit class JsonOps(val json: Json) extends AnyVal {
    def add(path: List[String], value: Json): Json = path match {
      case Nil => json
      case last :: Nil => json.withObject(_ + (last, value))
      case head :: tail => json.obj.fold(json)(obj => {
        Json.jObject(obj + (head, obj(head).getOrElse(jEmptyObject).add(tail, value)))
      })
    }
  }
}