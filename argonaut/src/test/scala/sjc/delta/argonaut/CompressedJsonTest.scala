package sjc.delta.argonaut

import org.junit.Test
import sjc.delta.Delta.DeltaOps
import sjc.delta.argonaut.json.beforeAfter.compressed.{encodeJsonToDelta, jsonDelta}


class CompressedJsonTest extends JsonTestUtil {
  @Test def complexExample(): Unit = {
    complexBefore delta complexAfter jsonShouldEqual """{
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
    Person(11, "bob", Dog(1, "fido")) delta Person(22, "sue", Dog(2, "rover")) jsonShouldEqual """{
    |  "age" :  { "before" : 11,    "after" : 22    },
    |  "name" : { "before" : "bob", "after" : "sue" },
    |  "pet" : {
    |    "age" :  { "before" : 1,      "after" : 2      },
    |    "name" : { "before" : "fido", "after" : "rover" }
    |  }
    |}""".stripMargin
  }
}
