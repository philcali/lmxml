package lmxml
package converters
package json

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class JsonConversionTest extends FlatSpec with ShouldMatchers {
  "JSON Conversion" should "be simple" in {
    val source = """
author @name="Philip Cali"
  interests arr "Hiking" "Coding" "Music"

people arr
    obj #1 @name="Anna Cali"
    obj #2 @name="Creed"
"""

    val expected =
    """{"author" : {"name" : "Philip Cali",""" +
    """ "interests" : ["Hiking", "Coding", "Music"]},""" +
    """ "people" : [{"id" : 1, "name" : "Anna Cali"},""" +
    """ {"id" : 2, "name" : "Creed"}]}"""

    val format = JsonConvert andThen (_.toString())

    DefaultLmxmlParser.fullParse(source)(format) should be === expected
  }
}
