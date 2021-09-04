package sjc.delta.circe

import io.circe.{Printer ⇒ PrettyParams, Encoder ⇒ EncodeJson, Json, JsonObject}
import io.circe.Json.{fromString ⇒ jString}
import sjc.delta.Delta.Aux
import sjc.delta.std.list.patience.{Removed, Equal, Inserted, Replaced}
import sjc.delta.{Patch, Delta}

import scala.reflect.ClassTag


object json extends json("left", "right", false) {
  object beforeAfter    extends json("before", "after", false)
  object actualExpected extends json("actual", "expected", false)
}

case class json(lhsName: String, rhsName: String, rfc6901Escaping: Boolean) { json ⇒
  object flat extends JsonDelta {
    def delta(left: Json, right: Json): Json = Json.obj(
      changes(left, right).map { case (pointer, change) ⇒ pointer.asString → flatten(change) }: _*
    )
  }

  object compressed extends JsonDelta {
    def delta(left: Json, right: Json): Json = changes(left, right).foldLeft(jEmptyObject) {
      case (acc, (Pointer(path), change)) ⇒ add(acc, path, flatten(change))
    }

    private def add(json: Json, path: List[String], value: Json): Json = path match { // TODO: make tail recursive
      case Nil          ⇒ json
      case last :: Nil  ⇒ json.mapObject(o ⇒ o add (last, value))
      case head :: tail ⇒ json.mapObject(o ⇒ o add (head, add(o.apply(head).getOrElse(jEmptyObject), tail, value)))
    }
  }

  object rfc6902 extends JsonDelta {
    def delta(left: Json, right: Json): Json = Json.arr(changes(left, right) map { // TODO: Add 'move' & 'copy'
      case (pointer, Add(rightJ))        ⇒ op(pointer, "add",     Json.obj("value" → rightJ))
      case (pointer, Remove(leftJ))      ⇒ op(pointer, "remove",  jEmptyObject)
      case (pointer, Replace(_, rightJ)) ⇒ op(pointer, "replace", Json.obj("value" → rightJ))
    }: _*)

    private def op(pointer: Pointer, op: String, json: Json): Json =
      json.mapObject((obj: JsonObject) ⇒ ("op" → jString(op)) +: ("path" → pointer.jString) +: obj)
  }

  private def changes(leftJ: Json, rightJ: Json)(implicit deltaJ: Aux[Json, Json]): List[(Pointer, Change)] = {
    def recurse(pointer: Pointer, left: Option[Json], right: Option[Json]): List[(Pointer, Change)] = {
      if (left == right) Nil else (left, right) match {
        case (Some(JObject(leftO: JsonObject)), Some(JObject(rightO))) ⇒ {
          (leftO.keys.toSet ++ rightO.keys.toSet).toList.flatMap(field ⇒ {
            recurse(pointer + field, leftO.apply(field), rightO.apply(field))
          })
        }
        case (Some(JArray(leftA)), Some(JArray(rightA)))               ⇒ {
          sjc.delta.std.list.patience.deltaList[Json].apply(leftA, rightA) flatMap {
            case Removed(subSeq, removed) ⇒ subSeq.leftRange.zip(removed) flatMap {
              case (index, item) ⇒ (pointer + index).change(Some(item), None)
            }
            case Inserted(subSeq, inserted) ⇒ subSeq.rightRange.zip(inserted) flatMap {
              case (index, item) ⇒ (pointer + index).change(None, Some(item))
            }
            case Replaced(subSeq, removed, inserted) ⇒ subSeq.leftRange.zip(removed.zip(inserted)) flatMap {
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

  private def flatten(change: Change): Json = change match {
    case Add(right)           ⇒ Json.fromJsonObject(missing(lhsName)  +: (rhsName → right) +: JsonObject.empty)
    case Remove(left)         ⇒ Json.fromJsonObject((lhsName → left)  +: missing(rhsName)  +: JsonObject.empty)
    case Replace(left, right) ⇒ Json.fromJsonObject((lhsName → left)  +: (rhsName → right) +: JsonObject.empty)
  }

  private def missing(name: String): (String, Json) = s"$name-missing" → Json.True

  private sealed trait Change
  private case class Add(rightJ: Json)                  extends Change
  private case class Remove(leftJ: Json)                extends Change
  private case class Replace(leftJ: Json, rightJ: Json) extends Change

  private case class Pointer(elements: List[String]) { // http://tools.ietf.org/html/rfc6901
    def +(index: Int): Pointer = this + index.toString
    def +(element: String): Pointer = copy(element :: elements)

    def change(leftOJ: Option[Json], rightOJ: Option[Json]): List[(Pointer, Change)] = (leftOJ, rightOJ) match {
      case (None,                None) ⇒ Nil
      case (Some(leftJ),         None) ⇒ List(reverse → Remove(leftJ))
      case (None,        Some(rightJ)) ⇒ List(reverse → Add(rightJ))
      case (Some(leftJ), Some(rightJ)) ⇒ List(reverse → Replace(leftJ, rightJ))
    }

    def jString: Json = Json.fromString(asString)
    def asString: String = if (elements.isEmpty) "" else "/" + elements.map(escape).mkString("/")

    private def escape(element: String): String = if (!rfc6901Escaping && element.startsWith("/")) s"[$element]" else {
      element.replaceAllLiterally("~", "~0").replaceAllLiterally("/", "~1")
    }

    private def reverse: Pointer = copy(elements.reverse)
  }

  private val jEmptyObject: Json = Json.fromJsonObject(JsonObject.empty)

  private object JObject { def unapply(json: Json): Option[JsonObject] = json.asObject   }
  private object JArray  { def unapply(json: Json): Option[List[Json]] = json.asArray.map(_.toList) }
}

trait JsonDelta {
  implicit def encodeJsonToDelta[A: EncodeJson]: Delta.Aux[A, Json] = jsonDelta.contramap[A](EncodeJson[A].apply)

  implicit val jsonDelta: Delta.Aux[Json, Json] with Patch[Json, String] = new Delta[Json] with Patch[Json, String] {
    type Out = Json

    def apply(left: Json, right: Json): Out = delta(left, right)
    def isEmpty(json: Json): Boolean = json == jEmptyObject
    def ignore(json: Json, paths: String*): Json = json.mapObject(obj ⇒ paths.foldLeft(obj)(_ remove _))
    def pretty(json: Json): String = PrettyParams.spaces2.print(json)

    protected val classTag: ClassTag[Json] = implicitly[ClassTag[Json]]
  }

  def delta(left: Json, right: Json): Json

  private val jEmptyObject: Json = Json.fromJsonObject(JsonObject.empty)
}