package sjc.delta.argonaut

import argonaut.{EncodeJson, Json, JsonObject}
import argonaut.Json.{jEmptyObject, jString}
import sjc.delta.Delta.Aux
import sjc.delta.std.list.patience.{Removed, Equal, Inserted, Replaced}
import sjc.delta.{Patch, Delta}


object json extends json("left", "right", false, true) {
  object beforeAfter    extends json("before", "after", false, true)
  object actualExpected extends json("actual", "expected", false, true)

  def excludeMissing: json = copy(includeMissing = false)
}

case class json(lhsName: String, rhsName: String, rfc6901Escaping: Boolean, includeMissing: Boolean) { json ⇒
  private val flattener = Flattener.Simple(lhsName, rhsName, includeMissing) // TODO: Make this configurable

  object flat extends JsonDelta { // TODO: Probably make these JsonDelta instances into vals
    def delta(left: Json, right: Json): Json = Json.jObjectFields(
      changes(left, right).map { case (pointer, change) ⇒ pointer.asString → flattener.flatten(change) }: _*
    )
  }

  object compressed extends JsonDelta {
    def delta(left: Json, right: Json): Json = changes(left, right).foldLeft(jEmptyObject) {
      case (acc, (Pointer(path), change)) ⇒ add(acc, path, flattener.flatten(change))
    }

    private def add(json: Json, path: List[String], value: Json): Json = path match { // TODO: make tail recursive
      case Nil          ⇒ json
      case last :: Nil  ⇒ json.withObject(o ⇒ o + (last, value))
      case head :: tail ⇒ json.withObject(o ⇒ o + (head, add(o.apply(head).getOrElse(jEmptyObject), tail, value)))
    }
  }

  object rfc6902 extends JsonDelta {
    def delta(left: Json, right: Json): Json = Json.jArrayElements(changes(left, right) map { // TODO: Add 'move' & 'copy'
      case (pointer, Change.Add(rightJ))        ⇒ op(pointer, "add",     ("value" → rightJ) ->: jEmptyObject)
      case (pointer, Change.Remove(leftJ))      ⇒ op(pointer, "remove",                         jEmptyObject)
      case (pointer, Change.Replace(_, rightJ)) ⇒ op(pointer, "replace", ("value" → rightJ) ->: jEmptyObject)
    }: _*)

    private def op(pointer: Pointer, op: String, obj: Json) = ("op" → jString(op)) ->: ("path" → pointer.jString) ->: obj
  }

  private def changes(leftJ: Json, rightJ: Json)(implicit deltaJ: Aux[Json, Json]): List[(Pointer, Change)] = {
    def recurse(pointer: Pointer, left: Option[Json], right: Option[Json]): List[(Pointer, Change)] = {
      if (left == right) Nil else (left, right) match {
        case (Some(JObject(leftO)), Some(JObject(rightO))) ⇒ {
          (leftO.fieldSet ++ rightO.fieldSet).toList.flatMap(field ⇒ {
            recurse(pointer + field, leftO.apply(field), rightO.apply(field))
          })
        }
        case (Some(JArray(leftA)), Some(JArray(rightA))) ⇒ { // TODO: make this configurable (should be able to use a naive list delta if you want
          sjc.delta.std.list.patience.deltaList[Json].apply(leftA, rightA) flatMap {
            case Removed(subSeq, removed) ⇒ subSeq.left.zip(removed) flatMap {
              case (index, item) ⇒ (pointer + index).change(Some(item), None)
            }
            case Inserted(subSeq, inserted) ⇒ subSeq.right.zip(inserted) flatMap {
              case (index, item) ⇒ (pointer + index).change(None, Some(item))
            }
            case Replaced(subSeq, removed, inserted) ⇒ subSeq.left.zip(removed.zip(inserted)) flatMap {
              case (index, (rem, ins)) ⇒ recurse(pointer + index, Some(rem), Some(ins))
            }
            case Equal(_, _) ⇒ Nil
          }
        }
        case _ ⇒ pointer.change(left, right)
      }
    }

    recurse(Pointer(Nil), Some(leftJ), Some(rightJ))
  }

  private case class Pointer(elements: List[String]) { // http://tools.ietf.org/html/rfc6901
    def +(index: Int): Pointer = this + index.toString
    def +(element: String): Pointer = copy(element :: elements)

    def change(leftOJ: Option[Json], rightOJ: Option[Json]): List[(Pointer, Change)] = (leftOJ, rightOJ) match {
      case (None,                None) ⇒ Nil
      case (Some(leftJ),         None) ⇒ List(reverse → Change.Remove(leftJ))
      case (None,        Some(rightJ)) ⇒ List(reverse → Change.Add(rightJ))
      case (Some(leftJ), Some(rightJ)) ⇒ List(reverse → Change.Replace(leftJ, rightJ))
    }

    def jString: Json = Json.jString(asString)
    def asString: String = if (elements.isEmpty) "" else "/" + elements.map(escape).mkString("/")

    private def escape(element: String) = if (!rfc6901Escaping && element.startsWith("/")) s"[$element]" else {
      element.replaceAllLiterally("~", "~0").replaceAllLiterally("/", "~1")
    }

    private def reverse = copy(elements.reverse)
  }

  private object JObject { def unapply(json: Json): Option[JsonObject] = json.obj   }
  private object JArray  { def unapply(json: Json): Option[List[Json]] = json.array }
}

trait JsonDelta {
  implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] = jsonDelta.contramap[A](EncodeJson.of[A].encode)

  implicit val jsonDelta: Delta.Aux[Json, Json] with Patch[Json] = new Delta[Json] with Patch[Json] {
    type Out = Json

    def apply(left: Json, right: Json): Out = delta(left, right)
    def isEmpty(json: Json): Boolean = json == Json.jEmptyObject
  }

  def delta(left: Json, right: Json): Json
}

sealed trait Change

object Change {
  case class Add(rightJ: Json)                  extends Change
  case class Remove(leftJ: Json)                extends Change
  case class Replace(leftJ: Json, rightJ: Json) extends Change
}

trait Flattener {
  def apply(change: Change): Json
}

object Flattener {
  case class Simple(lhsName: String, rhsName: String, includeMissing: Boolean) extends Flattener {
    def flatten(change: Change): Json = change match {
      case Change.Add(right)           ⇒ lhsMissing        ->?: (rhsName → right) ->:  jEmptyObject
      case Change.Remove(left)         ⇒ (lhsName → left)  ->:  rhsMissing        ->?: jEmptyObject
      case Change.Replace(left, right) ⇒ (lhsName → left)  ->:  (rhsName → right) ->:  jEmptyObject
    }

    private val lhsMissing = if (includeMissing) Some(s"$lhsName-missing" → Json.jTrue) else None
    private val rhsMissing = if (includeMissing) Some(s"$rhsName-missing" → Json.jTrue) else None
  }
}
