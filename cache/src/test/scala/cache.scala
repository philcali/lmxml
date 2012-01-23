package lmxml
package cache
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import java.io.{
  ByteArrayOutputStream,
  ByteArrayInputStream
}

class CacheTest extends FlatSpec with ShouldMatchers {
  // File testing
  val mockLocation = new java.io.File("test-cache")

  object LmxmlCache extends FileHashes with LmxmlFactory {
    def createParser(step: Int) = PlainLmxmlParser(step)

    val location = mockLocation
  }

  // Memory testing
  object LmxmlMemory extends HashLogic[String] with LmxmlFactory {
    def createParser(step: Int) = PlainLmxmlParser(step)

    val map = new scala.collection.mutable.HashMap[String, Array[Byte]]

    def save(contents: String) {
      val parser = apply(contents)

      val buffer = writeNodes(contents, parser.parseNodes(contents))

      val stream = buffer.asInstanceOf[ByteArrayOutputStream]

      map(hashString(contents)) = stream.toByteArray
    }

    def get(contents: String) = readNodes(contents)

    def changed(contents: String) = !map.contains(hashString(contents))

    def clear = map.clear
    
    def inStream(contents: String) = {
      val buf = map(hashString(contents))
      new ByteArrayInputStream(buf)
    }

    def outStream(contents: String) =
      new ByteArrayOutputStream()
  }

  def write(contents: String) {
    val writer = new java.io.FileWriter(new java.io.File("test.lmxml"))
    writer.write(contents)
    writer.close()
  }

  val contents = """dude @oi="true"
  smaller-dude
"""

  val expected = List(
    LmxmlNode("dude", Map("oi" -> "true"), List(
      LmxmlNode("smaller-dude")
    ))
  )

  def time(block: => Unit) = {
    val start = System.currentTimeMillis
    block
    val end = System.currentTimeMillis

    end - start
  }

  "Memory cache" should "be faster than parsing" in {

    val parser = LmxmlMemory(contents)

    // Prime object cache
    LmxmlMemory.save(contents)

    val parsing = time {
      (1 to 1000).foreach(_ => parser.parseNodes(contents))
    }

    val local = time {
      (1 to 1000).foreach(_ => LmxmlMemory.get(contents))
    }

    parsing should be > local
  }

  "Local cache" should "be a transparent file loading mixin" in {
    write(contents)
    
    LmxmlCache.fromFile("test.lmxml")(XmlConvert)

    val parsing = time {
      (1 to 1000).foreach(_ => Lmxml.fromFile("test.lmxml")(XmlConvert))
    }

    val local = time {
      (1 to 1000).foreach(_ => LmxmlCache.fromFile("test.lmxml")(XmlConvert))
    }

    parsing should be > local

    LmxmlCache.clear

    new java.io.File("test.lmxml").delete
  }
} 
