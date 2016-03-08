package sjc.delta.argonaut

import argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.junit.Test
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.beforeAfter.flat.{encodeJsonToDelta, jsonDelta}


class FlatJsonTest extends JsonTestUtil {
  @Test def shouldIgnoreIdenticalElements(): Unit = {
    jNull          delta jNull          jsonShouldEqual jEmptyObject
    jBool(true)    delta jBool(true)    jsonShouldEqual jEmptyObject
    jNumber(123)   delta jNumber(123)   jsonShouldEqual jEmptyObject
    jString("abc") delta jString("abc") jsonShouldEqual jEmptyObject
    jEmptyObject   delta jEmptyObject   jsonShouldEqual jEmptyObject
    jArray(Nil)    delta jArray(Nil)    jsonShouldEqual jEmptyObject

    parse("""["abc"]""")                      delta parse("""["abc"]""")                      jsonShouldEqual jEmptyObject
    parse("""{"foo": "abc"}""")               delta parse("""{"foo": "abc"}""")               jsonShouldEqual jEmptyObject
    parse("""{"foo": "abc", "bar": "abc"}""") delta parse("""{"foo": "abc", "bar": "abc"}""") jsonShouldEqual jEmptyObject
  }

  @Test def shouldListDifferentElements(): Unit =  {
    jString("abc")       delta jString("def")       jsonShouldEqual """{"": {"before": "abc", "after": "def"}}"""
    parse("""["abc"]""") delta parse("""["def"]""") jsonShouldEqual """{"/0": {"before": "abc", "after": "def"}}"""

    parse("""{"foo": "abc"}""") delta parse("""{"foo": "def"}""") jsonShouldEqual
      """{"/foo": {"before": "abc", "after": "def"}}"""
  }

  @Test def shouldListMissingElements(): Unit =  {
    parse("{}") delta parse("""{"parent": "def"}""") jsonShouldEqual """{"/parent": {"after": "def"}}"""
    parse("[]") delta parse("""["def"]""")           jsonShouldEqual """{"/0": {"after": "def"}}"""
  }

  @Test def shouldListExtraElements(): Unit = {
    parse("""{"parent": "def"}""") delta parse("{}") jsonShouldEqual """{"/parent": {"before": "def"}}"""
    parse("""["def"]""")           delta parse("[]") jsonShouldEqual """{"/0": {"before": "def"}}"""
  }

  @Test def complexExample(): Unit = {
    complexBefore delta complexAfter jsonShouldEqual """{
    |  "/items/0/author" : {
    |    "before" : "somebody@flickr.com (Talk On Travel)",
    |    "after"  : "nobody@flickr.com (Talk On Travel)"
    |  },
    |  "/items/0/published" : {
    |    "before" : "2008-12-04T12:43:03Z",
    |    "after"  : "2016-12-04T12:43:03Z"
    |  },
    |  "/link" : {
    |    "before" : "http://www.flickr.com/groups/talkontravel/pool/",
    |    "after"  : "http://www.flickr.com/groups/talkontravel/bar/"
    |  },
    |  "/title" : {
    |    "before" : "Talk On Travel Pool",
    |    "after"  : "Talk On Travel Bar"
    |  }
    |}""".stripMargin
  }

  @Test def genericDelta(): Unit = {
    Person(11, "bob", Dog(1, "fido")) delta Person(22, "sue", Dog(2, "rover")) jsonShouldEqual """{
    |  "/age":      { "before": 11,     "after": 22       },
    |  "/name":     { "before": "bob",  "after": "sue"    },
    |  "/pet/age":  { "before": 1,      "after" : 2       },
    |  "/pet/name": { "before": "fido", "after" : "rover" }
    |}""".stripMargin
  }
}