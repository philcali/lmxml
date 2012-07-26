package lmxml
package converters
package json

import transforms.{Transform, Foreach, Value}

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JsonConversionTest extends FlatSpec with ShouldMatchers {
  "JSON Conversion" should "be simple" in {
    val source = """
author @name="Philip Cali"
  interests arr interests-loop interest

people arr
    obj #1 @name="Anna Cali"
    obj #2 @name="Creed"
"""

    val expected = """{
  "author" : {
    "name" : "Philip Cali",
    "interests" : ["Hiking", "Coding", "Music"]
  },
  "people" : [{
    "id" : 1,
    "name" : "Anna Cali"
  }, {
    "id" : 2,
    "name" : "Creed"
  }]
}"""

    val trans = Transform(
      "interests-loop" -> Foreach(List("Hiking", "Coding", "Music")) { i => Seq(
        "interest" -> Value(i)
      )}
    )

    val format = trans andThen JsonConvert andThen JsonFormat

    DefaultLmxmlParser.fullParse(source)(format) should be === expected
  }
}
