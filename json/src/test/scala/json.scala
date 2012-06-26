package lmxml
package transforms
package json
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JSONTest extends FlatSpec with ShouldMatchers {
  val source = """
test
  h1 will
  p "Various degrees of: {determination}"

ul .people
  people
    li "{id}: {firstname} {lastname}"
"""

  "JSON" should "be dynamically templated" in {
    val test = """{
  "test": {
    "will": "strong",
    "determination": "good"
  },
  "people": [
    { "id": 1, "firstname": "Philip", "lastname": "Cali" },
    { "id": 2, "firstname": "Anna", "lastname": "Cali" }
  ]
}"""

    val expected = List(
      TextNode("", children = List(
        LmxmlNode("h1", children = List(TextNode("strong"))),
        LmxmlNode("p", children = List(
          TextNode("Various degrees of: good")
        ))
      )),
      LmxmlNode("ul", Map("class" -> "people"), List(
        TextNode("", children = List(
          LmxmlNode("li", children = List(
            TextNode("1: Philip Cali")
          )),
          LmxmlNode("li", children = List(
            TextNode("2: Anna Cali")
          ))
        ))
      ))
    )

    import JSTransform.Filters._
    val transform = JSTransform("people" -> onArray(_.size > 1)).parse(test)

    DefaultLmxmlParser.fullParse(source)(transform) should be === expected
  }
}
