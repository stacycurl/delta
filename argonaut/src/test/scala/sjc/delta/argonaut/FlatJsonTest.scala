package sjc.delta.argonaut

import argonaut.CodecJson
import argonaut.Json.{jArray, jBool, jEmptyObject, jNull, jNumber, jString}
import org.junit.Test
import sjc.delta.Delta.DeltaOps


class FlatJsonTest extends JsonTestUtil {
  import sjc.delta.argonaut.json.beforeAfter.flat.jsonDelta

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
    parse("""["abc"]""") delta parse("""["def"]""") jsonShouldEqual """{"0": {"before": "abc", "after": "def"}}"""

    parse("""{"foo": "abc"}""") delta parse("""{"foo": "def"}""") jsonShouldEqual
      """{"foo": {"before": "abc", "after": "def"}}"""
  }

  @Test def shouldListMissingElements(): Unit =  {
    parse("{}") delta parse("""{"parent": "def"}""") jsonShouldEqual """{"parent": {"after": "def"}}"""
    parse("[]") delta parse("""["def"]""")           jsonShouldEqual """{"0": {"after": "def"}}"""
  }

  @Test def shouldListExtraElements(): Unit = {
    parse("""{"parent": "def"}""") delta parse("{}") jsonShouldEqual """{"parent": {"before": "def"}}"""
    parse("""["def"]""")           delta parse("[]") jsonShouldEqual """{"0": {"before": "def"}}"""
  }

  @Test def genericDelta(): Unit = {
    import sjc.delta.argonaut.json.beforeAfter.flat.encodeJsonToDelta

    case class Person(age: Int, name: String)
    implicit val codecPerson: CodecJson[Person] = CodecJson.casecodec2(Person.apply, Person.unapply)("age", "name")

    Person(1, "foo") delta Person(2, "bar") jsonShouldEqual
      """{"age": {"before": 1, "after": 2}, "name": {"before": "foo", "after": "bar"}}"""
  }
}

class CompressedJsonTest extends JsonTestUtil {
  import sjc.delta.argonaut.json.beforeAfter.compressed.jsonDelta

  @Test def complexExample(): Unit = {
    (parse("""{"person": {"name": "bob", "age": 30}}""") delta
    parse("""{"person": {"name": "sue", "age": 29}}""")) jsonShouldEqual
    """{
      |  "person" : {
      |    "name" : {
      |      "before" : "bob",
      |      "after" : "sue"
      |    },
      |    "age" : {
      |      "before" : 30,
      |      "after" : 29
      |    }
      |  }
      |}""".stripMargin
  }

  @Test def complexExample2(): Unit = {
    val before = parse(
      """
        |{
        |    "title": "Talk On Travel Pool",
        |    "link": "http://www.flickr.com/groups/talkontravel/pool/",
        |    "description": "Travel and vacation photos from around the world.",
        |    "modified": "2009-02-02T11:10:27Z",
        |    "generator": "http://www.flickr.com/",
        |    "items": [
        |            {
        |            "title": "View from the hotel",
        |            "link": "http://www.flickr.com/photos/33112458@N08/3081564649/in/pool-998875@N22",
        |            "media": {"m":"http://farm4.static.flickr.com/3037/3081564649_4a6569750c_m.jpg"},
        |            "date_taken": "2008-12-04T04:43:03-08:00",
        |            "description": " Talk On Travel< /a> has added a photo to the pool:< /p>
        | < /a>< /p> ",
        |            "published": "2008-12-04T12:43:03Z",
        |            "author": "somebody@flickr.com (Talk On Travel)",
        |            "author_id": "33112458@N08",
        |            "tags": "spain dolphins tenerife canaries lagomera aqualand playadelasamericas junglepark losgigantos loscristines talkontravel"
        |            }
        |    ]
        |}
      """.stripMargin
    )

    val after = parse(
      """
        |{
        |    "title": "Talk On Travel Bar",
        |    "link": "http://www.flickr.com/groups/talkontravel/bar/",
        |    "description": "Travel and vacation photos from around the world.",
        |    "modified": "2009-02-02T11:10:27Z",
        |    "generator": "http://www.flickr.com/",
        |    "items": [
        |            {
        |            "title": "View from the hotel",
        |            "link": "http://www.flickr.com/photos/33112458@N08/3081564649/in/pool-998875@N22",
        |            "media": {"m":"http://farm4.static.flickr.com/3037/3081564649_4a6569750c_m.jpg"},
        |            "date_taken": "2008-12-04T04:43:03-08:00",
        |            "description": " Talk On Travel< /a> has added a photo to the pool:< /p>
        | < /a>< /p> ",
        |            "published": "2016-12-04T12:43:03Z",
        |            "author": "nobody@flickr.com (Talk On Travel)",
        |            "author_id": "33112458@N08",
        |            "tags": "spain dolphins tenerife canaries lagomera aqualand playadelasamericas junglepark losgigantos loscristines talkontravel"
        |            }
        |    ]
        |}
      """.stripMargin)

    before delta after jsonShouldEqual
      """{
        |  "items" : {
        |    "0" : {
        |      "author" : {
        |        "before" : "somebody@flickr.com (Talk On Travel)",
        |        "after" : "nobody@flickr.com (Talk On Travel)"
        |      },
        |      "published" : {
        |        "before" : "2008-12-04T12:43:03Z",
        |        "after" : "2016-12-04T12:43:03Z"
        |      }
        |    }
        |  },
        |  "link" : {
        |    "before" : "http://www.flickr.com/groups/talkontravel/pool/",
        |    "after" : "http://www.flickr.com/groups/talkontravel/bar/"
        |  },
        |  "title" : {
        |    "before" : "Talk On Travel Pool",
        |    "after" : "Talk On Travel Bar"
        |  }
        |}""".stripMargin
  }

  @Test def genericDelta(): Unit = {
    import sjc.delta.argonaut.json.beforeAfter.compressed.encodeJsonToDelta

    case class Person(age: Int, name: String, pet: Dog)
    case class Dog(age: Int, name: String)
    implicit val codecDog: CodecJson[Dog] = CodecJson.casecodec2(Dog.apply, Dog.unapply)("age", "name")
    implicit val codecPerson: CodecJson[Person] = CodecJson.casecodec3(Person.apply, Person.unapply)("age", "name", "pet")

    Person(11, "bob", Dog(1, "fido")) delta Person(22, "sue", Dog(2, "rover")) jsonShouldEqual
      """{
        |  "age" :  { "before" : 11,    "after" : 22    },
        |  "name" : { "before" : "bob", "after" : "sue" },
        |  "pet" : {
        |    "age" :  { "before" : 1,      "after" : 2      },
        |    "name" : { "before" : "fido", "after" : "rover" }
        |  }
        |}""".stripMargin
  }
}

