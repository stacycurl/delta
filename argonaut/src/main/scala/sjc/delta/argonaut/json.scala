package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, JsonObject}
import argonaut.Json.{jEmptyObject, jString}
import sjc.delta.Delta

object json extends json("left", "right") {
  object beforeAfter    extends json("before", "after")
  object actualExpected extends json("actual", "expected")
}

case class json(lhsName: String, rhsName: String) { json =>
  object flat {
    implicit val jsonDelta: Delta.Aux[Json, Json] =
      Delta.from[Json].curried[Json](left => right => deltas(left, right))

    implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] = jsonDelta.contramap[A](EncodeJson.of[A].encode)

    def deltas(leftJ: Json, rightJ: Json): Json =
      Json.jObjectFields(changes(leftJ, rightJ).map { case (pointer, change) ⇒ pointer.asString → flatten(change) }: _*)
  }

  object compressed {
    import util.JsonOps

    implicit val jsonDelta: Delta.Aux[Json, Json] = Delta.from[Json].curried[Json](left ⇒ right ⇒ {
      changes(left, right).foldLeft(jEmptyObject) {
        case (acc, (pointer, change)) ⇒ acc.add(pointer.elements, flatten(change))
      }
    })

    implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] = jsonDelta.contramap[A](EncodeJson.of[A].encode)
  }

  object rfc6902 {
    implicit val jsonDelta: Delta.Aux[Json, Json] = Delta.from[Json].curried[Json](left ⇒ right ⇒ {
      def op(pointer: Pointer, op: String) = ("path" → pointer.jString) ->: ("op" → jString("add")) ->: jEmptyObject

      // TODO: Add 'move' & 'copy'
      Json.jArrayElements(changes(left, right) map {
        case (pointer, Add(rightJ))        ⇒ ("value" → rightJ) ->: op(pointer, "add")
        case (pointer, Remove(leftJ))      ⇒ op(pointer, "remove")
        case (pointer, Replace(_, rightJ)) ⇒ ("value" → rightJ) ->: op(pointer, "replace")
      }: _*)
    })
  }

  private def changes(leftJ: Json, rightJ: Json): List[(Pointer, Change)] = {
    def recurse(pointer: Pointer, left: Option[Json], right: Option[Json]): List[(Pointer, Change)] = {
      if (left == right) Nil else (left, right) match {
        case (Some(JObject(leftO)), Some(JObject(rightO))) ⇒ {
          val fields = (leftO.fieldSet ++ rightO.fieldSet).toList

          fields.flatMap(field ⇒ {
            recurse(pointer + field, leftO.apply(field), rightO.apply(field))
          })
        }
        case (Some(JArray(leftA)), Some(JArray(rightA))) ⇒ {
          // TODO: This will become better once a Delta[List[A]] (using a 'patience diff' or otherwise) is done.
          Range(0, leftA.length max rightA.length).toList.flatMap(index ⇒ {
            recurse(pointer + index.toString, leftA.lift(index), rightA.lift(index))
          })
        }
        case _ ⇒ pointer.change(left, right)
      }
    }

    recurse(Pointer(Nil), Some(leftJ), Some(rightJ))
  }

  private def flatten(change: Change): Json = change match {
    case Add(right)           ⇒ (rhsName -> right) ->: jEmptyObject
    case Remove(left)         ⇒ (lhsName -> left)  ->: jEmptyObject
    case Replace(left, right) ⇒ (lhsName -> left)  ->: (rhsName -> right) ->: jEmptyObject
  }

  private sealed trait Change
  private case class Add(rightJ: Json) extends Change
  private case class Remove(leftJ: Json) extends Change
  private case class Replace(leftJ: Json, rightJ: Json) extends Change

  // This is almost a JSON Pointer (RFC 6901) the only difference (I think) is not escaping '/' or '~'
  private case class Pointer(elements: List[String]) {
    def +(element: String): Pointer = copy(element :: elements)

    def change(leftOJ: Option[Json], rightOJ: Option[Json]): List[(Pointer, Change)] = (leftOJ, rightOJ) match {
      case (None,                None) ⇒ Nil
      case (Some(leftJ),         None) ⇒ List(reverse → Remove(leftJ))
      case (None,        Some(rightJ)) ⇒ List(reverse → Add(rightJ))
      case (Some(leftJ), Some(rightJ)) ⇒ List(reverse → Replace(leftJ, rightJ))
    }

    def jString: Json = Json.jString(asString)
    def asString: String = if (elements.isEmpty) "" else elements.mkString("/")

    private def reverse = copy(elements.reverse)
  }

  private object JObject { def unapply(json: Json): Option[JsonObject] = json.obj   }
  private object JArray  { def unapply(json: Json): Option[List[Json]] = json.array }
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