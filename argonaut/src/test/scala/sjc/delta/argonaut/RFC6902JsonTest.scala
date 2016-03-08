package sjc.delta.argonaut

import org.junit.Test
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.rfc6902.{encodeJsonToDelta, jsonDelta}


class RFC6902JsonTest extends JsonTestUtil {
  @Test def add(): Unit = {
    parse("{}") delta parse("""{"parent": "def"}""") jsonShouldEqual """[{"op": "add", "path": "/parent", "value": "def"}]"""
    parse("[]") delta parse("""["def"]""")           jsonShouldEqual """[{"op": "add", "path": "/0", "value": "def"}]"""
  }

  @Test def remove(): Unit = {
    parse("""{"parent": "def"}""") delta parse("{}") jsonShouldEqual """[{"op" : "remove", "path" : "/parent"}]"""
    parse("""["def"]""")           delta parse("[]") jsonShouldEqual """[{"op" : "remove", "path" : "/0"}]"""
  }

  @Test def complexExample(): Unit = {
    complexBefore delta complexAfter jsonShouldEqual """[
    |  { "op": "replace", "path": "/items/0/author",    "value": "nobody@flickr.com (Talk On Travel)"             },
    |  { "op": "replace", "path": "/items/0/published", "value": "2016-12-04T12:43:03Z"                           },
    |  { "op": "replace", "path": "/link",              "value": "http://www.flickr.com/groups/talkontravel/bar/" },
    |  { "op": "replace", "path": "/title",             "value": "Talk On Travel Bar"                             }
    |]""".stripMargin
  }

  @Test def genericDelta(): Unit = {
    Person(11, "bob", Dog(1, "fido")) delta Person(22, "sue", Dog(2, "rover")) jsonShouldEqual """[
    |  { "op" : "replace", "path" : "/age",      "value" : 22     },
    |  { "op" : "replace", "path" : "/name",     "value" : "sue"  },
    |  { "op" : "replace", "path" : "/pet/age",  "value" : 2      },
    |  { "op" : "replace", "path" : "/pet/name", "value" : "rover"}
    |]""".stripMargin
  }
}