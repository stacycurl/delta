package sjc.delta.argonaut

import argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.scalatest.FreeSpec
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
    parse("{}") delta parse("""{"parent": "def"}""") jsonShouldEqual s"""{"/parent": {$beforeMissing, "after": "def"}}"""
    parse("[]") delta parse("""["def"]""")           jsonShouldEqual s"""{"/0": {$beforeMissing, "after": "def"}}"""
  }

  "should list extra elements" in {
    parse("""{"parent": "def"}""") delta parse("{}") jsonShouldEqual s"""{"/parent": {"before": "def", $afterMissing}}"""
    parse("""["def"]""")           delta parse("[]") jsonShouldEqual s"""{"/0": {"before": "def", $afterMissing}}"""
  }

  "complex list example" in {
    println((parse("""[4, 5, 6, 7, 8, 9, 10, 1000, 1001, 1002, 1003, 2000]""") delta parse("""[4, 77, 5, 6, 9, 100, 1000, 2000, 2004, 2005, 2006]""")).spaces2)

    parse("""[4, 5, 6, 7, 8, 9, 10, 1000, 1001, 1002, 1003, 2000]""") delta parse("""[4, 77, 5, 6, 9, 100, 1000, 2000, 2004, 2005, 2006]""") jsonShouldEqual s"""{
    |  "/[1]"  : { "added" : 77  },
    |  "/[3]"  : { "removed" : 7 },
    |  "/[4]"  : { "removed" : 8 },
    |  "/[6]"  : { "before" : 10,  "after" : 100  },
    |  "/[8]"  : { "added" : 2004 },
    |  "/[9]"  : { "added" : 2005 },
    |  "/[10]" : { "added" : 2006 }
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
    |  "/[0]" : {
    |    "before" : 0,
    |    "after" : 1
    |  }
    |}""".stripMargin

    parse("""[1, 2]""") delta parse("""[0, 1, 2]""") jsonShouldEqual """{
    |  "/[0]": {
    |    "added": 0
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

  private val beforeMissing = """"before-missing": true"""
  private val afterMissing  = """"after-missing": true"""
}