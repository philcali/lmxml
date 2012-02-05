package lmxml
package example

import transforms._
import template._
import cache.FileHashes

import unfiltered.request._
import unfiltered.response._
import unfiltered.jetty.Http

import java.io.File

object CustomFactory extends LmxmlFactory with FileHashes {
  val location = new File("example/cache")

  def createParser(step: Int) =
    new PlainLmxmlParser(step) with FileTemplate {
      val working = new File("example/data/templates")
    }
}

object ContactsApp extends unfiltered.filter.Plan {
  val base = new File("example/data/templates")

  lazy val list = Transform (
    "people" -> Foreach(Database.get) { person => Seq(
      "person-id" -> Value(person.id),
      "person-name" -> Value(person.fullname)
    )}
  )

  def intent = {
    case GET(Path("/")) =>
      val file = new File(base, "index.lmxml")
      val format = list andThen XmlConvert
      val response = CustomFactory.fromFile(file)(format)

      Status(200) ~>
      ContentType("text/html") ~>
      ResponseString(response.toString)

    case GET(Path("/new")) =>
      val file = new File(base, "new.lmxml")
      val response = CustomFactory.fromFile(file)(XmlConvert)
      
      Status(200) ~>
      ContentType("text/html") ~>
      ResponseString(response.toString)
  }
}

object Examples {
  def main(args: Array[String]) {
    Http(8080).filter(ContactsApp).run()
  }
}
