package sjc.delta.argonaut

import argonaut.{CodecJson, Json, Parse, PrettyParams}

import org.scalatest.Matchers


trait JsonTestUtil extends Matchers {
  implicit class JsonTestOps(json: Json) {
    def jsonShouldEqual(expected: String): Unit = json jsonShouldEqual parse(expected)
    def jsonShouldEqual(expected: Json): Unit = preserveOrder.pretty(json) shouldBe preserveOrder.pretty(expected)
  }

  def parse(content: String): Json = Parse.parse(content).valueOr(error ⇒ sys.error("not json: " + error))

  case class Person(age: Int, name: String, pet: Dog)
  case class Dog(age: Int, name: String)
  implicit val codecDog: CodecJson[Dog] = CodecJson.casecodec2(Dog.apply, Dog.unapply)("age", "name")
  implicit val codecPerson: CodecJson[Person] = CodecJson.casecodec3(Person.apply, Person.unapply)("age", "name", "pet")

  val complexBefore = parse(
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

  val complexAfter = parse(
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
    """.stripMargin
  )


  private val preserveOrder = PrettyParams.spaces2.copy(preserveOrder = true)
}