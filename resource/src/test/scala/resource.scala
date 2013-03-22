package lmxml
package resource
package test

import org.scalatest.{ FlatSpec, BeforeAndAfterAll }
import org.scalatest.matchers.ShouldMatchers

class FileResourcesTest extends FlatSpec with ShouldMatchers with BeforeAndAfterAll {
  val testFiles = Map(
    "file1" -> "This is what it's all about",
    "file2" -> "This is more about it",
    "file3" -> "This is the final straw"
  )

  object LoadFile extends PlainLmxmlParser(2) with FileResources {
    private val foundFiles = collection.mutable.ListBuffer[String]()

    def check(expected: List[String]) =
      foundFiles.toList should be === expected

    val resourceLoader = new ResourceLoader {
      def apply(attrs: Map[String, String]) = {
        foundFiles += attrs("file")
        defaultLoad(attrs)
      }
    }
  }

  object Resources extends PlainLmxmlParser(2) with FileResources {
    val resourceLoader = TextResource
  }

  override def beforeAll() {
    testFiles foreach {
      case (file, text) =>
        val writer = new java.io.FileWriter(file)
        writer.write(text)
        writer.close()
    }
  }

  override def afterAll() {
    testFiles.keys.map(new java.io.File(_)).foreach(_.delete)
  }

  val source = """
lmxml
  load @file="file1"
  load @file="file2"
  load @file="file3"
"""

  val expectedNodes = List(
    LmxmlNode("lmxml", children = Seq(
      TextNode(testFiles("file1")),
      TextNode(testFiles("file2")),
      TextNode(testFiles("file3"))
    ))
  )

  "TextResource" should "load text from files" in {
    testFiles foreach {
      case (file, text) =>
        val toplevel = TextResource(Map("file" -> file))
        toplevel(Nil).contents should be === text
    }
  }

  "FileResources" should "find physical file resources" in {
    LoadFile.parseNodes(source)

    LoadFile.check(testFiles.keys.toList)
  }

  it should "load text resources" in {
    val parsed = Resources parseNodes source

    parsed should be === expectedNodes
  }
}
