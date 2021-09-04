package sjc.delta.circe

import io.circe.{Json, JsonObject}
import org.scalatest.FreeSpec

import scala.xml.{Node, Text}


class RenderJsonSpec extends FreeSpec with JsonSpecUtil {
  "wip" in {
    val rendered = render(parse(
      """
        |{
        |  "key": [
        |    {
        |      "inner": 3,
        |      "value": false
        |    }
        |  ]
        |}
        |""".stripMargin))
  }

  def render(json: Json): Node = json match {
    case JObject(obj) ⇒
      <table>
        <tr><td>{Text("{")}</td><td colspan="3"></td></tr>
        {
          obj.toList.map {
            case (k, v) => <tr><td/><td>{k}</td><td>:</td><td>{render(v)}</td></tr>
          }
        }
        <tr><td>{Text("}")}</td><td colspan="3"></td></tr>
      </table>
    case JArray(elements) ⇒
      <table>
        <tr><td>[</td><td/></tr>
        {
          elements.map(element => {
            <tr><td/><td>{render(element)}</td></tr>
          })
        }
        <tr><td>]</td><td/></tr>
      </table>
    case _                ⇒ Text(json.noSpaces.toString)
  }

  private val JObject = Extractor[Json, JsonObject](_.asObject)
  private val JArray  = Extractor[Json, List[Json]](_.asArray.map(_.toList))

  trait Extractor[A, B] {
    def unapply(a: A): Option[B]
  }

  object Extractor {
    def apply[A, B](f: A ⇒ Option[B]): Extractor[A, B] = new Extractor[A, B] {
      def unapply(a: A): Option[B] = f(a)
    }
  }
}
