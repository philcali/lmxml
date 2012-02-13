package lmxml
package transforms
package json
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JSONTest extends FlatSpec with ShouldMatchers {
  val source = """
ul .people
  people
    li "{id}: {firstname} {lastname}"
"""

  "JSON" should "be dynamically templated" in {
    val test = """{
  "people": [
    { "id": 1, "firstname": "Philip", "lastname": "Cali" },
    { "id": 2, "firstname": "Anna", "lastname": "Cali" }
  ]
}"""

    val expected = List(
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

    val transform = JSTransform(test).getOrElse(Transform())

    DefaultLmxmlParser.fullParse(source)(transform) should be === expected
  }
}
