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
      if (left == right) Nil else (left, right) match {
        case (Some(JObject(leftO)), Some(JObject(rightO))) ⇒ {
          val fields = (leftO.fieldSet ++ rightO.fieldSet).toList

          fields.flatMap(field ⇒ {
            recurse(context + field, leftO.apply(field), rightO.apply(field))
          })
        }
        case (Some(JArray(leftA)), Some(JArray(rightA))) ⇒ {
          // TODO: This will become better once a Delta[List[A]] (using a 'patience diff' or otherwise) is done.
          Range(0, leftA.length max rightA.length).toList.flatMap(index ⇒ {
            recurse(context + index.toString, leftA.lift(index), rightA.lift(index))
          })
        }
        case _ ⇒ context.error(left, right)
      }
    }

    Json.jObjectFields(recurse(Context(Nil), Some(leftJ), Some(rightJ)): _*)
  }

  private object JObject { def unapply(json: Json): Option[JsonObject] = json.obj   }
  private object JArray  { def unapply(json: Json): Option[List[Json]] = json.array }

  private case class Context(elements: List[String]) {
    def +(element: String): Context = copy(element :: elements)

    def error(leftOJ: Option[Json], rightOJ: Option[Json]): List[(String, Json)] = List(
      pointer -> (leftOJ.map(lhsName -> _) ->?: rightOJ.map(rhsName -> _) ->?: jEmptyObject)
    )

    // This is almost a JSON Pointer (RFC 6901) the only difference (I think) is not escaping '/' or '~'
    private def pointer: String = if (elements.isEmpty) "" else elements.reverse.mkString("/")
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