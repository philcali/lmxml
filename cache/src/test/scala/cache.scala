package lmxml
package cache
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class CacheTest extends FlatSpec with ShouldMatchers {
  val mockLocation = new java.io.File("test-cache")

  object LocalStorage extends FileHashes with LmxmlFactory {
    val location = mockLocation

    def createParser(step: Int) = PlainLmxmlParser(step)
  }

  def write(contents: String) {
    val writer = new java.io.FileWriter(new java.io.File("test.lmxml"))
    writer.write(contents)
    writer.close()
  }

  "Local cache" should "store cache" in {
    val contents = """dude @oi="true"
  smaller-dude
"""

    val expected = List(
      LmxmlNode("dude", Map("oi" -> "true"), List(
        LmxmlNode("smaller-dude"))
      )
    )

    write(contents)

    LocalStorage.fromFile("test.lmxml") 
  }
} 
