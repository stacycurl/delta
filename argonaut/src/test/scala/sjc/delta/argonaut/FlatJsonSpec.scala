package sjc.delta.argonaut

import argonaut.Json
import argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.scalatest.FreeSpec
import org.scalatest.exceptions.TestFailedException
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.beforeAfter.flat.{encodeJsonToDelta, jsonDelta}


class FlatJsonSpec extends FreeSpec with JsonSpecUtil {
  "should ignore identical elements" in {
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

  "should list different elements" in {
    jString("abc")       delta jString("def")       jsonShouldEqual """{"": {"before": "abc", "after": "def"}}"""
    parse("""["abc"]""") delta parse("""["def"]""") jsonShouldEqual """{"/0": {"before": "abc", "after": "def"}}"""

    parse("""{"foo": "abc"}""") delta parse("""{"foo": "def"}""") jsonShouldEqual
      """{"/foo": {"before": "abc", "after": "def"}}"""
  }

  "should list missing elements" in {
    parse("{}") delta parse("""{"parent": "def"}""") jsonShouldEqual """{"/parent": {"before-missing": true, "after": "def"}}"""
    parse("[]") delta parse("""["def"]""")           jsonShouldEqual """{"/0": {"before-missing": true, "after": "def"}}"""
  }

  "should list extra elements" in {
    parse("""{"parent": "def"}""") delta parse("{}") jsonShouldEqual """{"/parent": {"before": "def", "after-missing": true}}"""
    parse("""["def"]""")           delta parse("[]") jsonShouldEqual """{"/0": {"before": "def", "after-missing": true}}"""
  }

  "complex list example" in {
    parse("""[4, 5, 6, 7, 8, 9, 10, 1000, 1001, 1002, 1003, 2000]""") delta parse("""[4, 77, 5, 6, 9, 100, 1000, 2000, 2004, 2005, 2006]""") jsonShouldEqual """{
    |  "/1"  : { "before-missing": true, "after" : 77          },
    |  "/3"  : { "before" : 7,           "after-missing": true },
    |  "/4"  : { "before" : 8,           "after-missing": true },
    |  "/6"  : { "before" : 10,          "after" : 100         },
    |  "/8"  : { "before-missing": true, "after" : 2004        },
    |  "/9"  : { "before-missing": true, "after" : 2005        },
    |  "/10" : { "before-missing": true, "after" : 2006        }
    }""".stripMargin
  }

  "complex example" in {
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
    }""".stripMargin

    parse("""[0, 2]""") delta parse("""[1, 2]""") jsonShouldEqual """{
    |  "/0" : {
    |    "before" : 0,
    |    "after" : 1
    |  }
    |}""".stripMargin
  }

  "generic delta" in {
    Person(11, "bob", Dog(1, "fido")) delta Person(22, "sue", Dog(2, "rover")) jsonShouldEqual """{
    |  "/age":      { "before": 11,     "after": 22       },
    |  "/name":     { "before": "bob",  "after": "sue"    },
    |  "/pet/age":  { "before": 1,      "after" : 2       },
    |  "/pet/name": { "before": "fido", "after" : "rover" }
    |}""".stripMargin
  }
}