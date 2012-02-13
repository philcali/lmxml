package lmxml
package example

import transforms._
import template._
import shortcuts.html.HtmlShortcuts
import cache.{
  FileHashes,
  FileStorage
}

import unfiltered.request._
import unfiltered.response._
import unfiltered.filter.request.ContextPath
import unfiltered.jetty.Http

import java.io.File

object Lmxml extends LmxmlFactory with FileHashes {
  val storage = new FileStorage(new File("example/cache"))

  def createParser(step: Int) =
    new PlainLmxmlParser(step) with FileTemplates with HtmlShortcuts {
      val working = new File("example/data/templates")
    }
}

object Resources extends unfiltered.filter.Plan {
  def pump(in: java.io.InputStream, out: java.io.OutputStream): Unit = {
    val bytes = new Array[Byte](1024)
    in.read(bytes) match {
      case n if n > 0 => out.write(bytes, 0, n); pump(in, out)
      case _ => in.close(); out.close()
    }
  }

  def intent = {
    case ContextPath(cp, p) =>
      val s = getClass.getResourceAsStream(cp + "/" + p)
      val bs = new java.io.ByteArrayOutputStream()

      pump(s, bs)

      Status(200) ~>
      ContentType(Mime.unapply(p).get) ~> 
      ResponseString(new String(bs.toByteArray, "UTF-8"))
  }
}

object ContactsApp extends unfiltered.filter.Plan {
  val base = new File("example/data/templates")

  def list = Transform (
    "people" -> Foreach(Database.getAll) { person => Seq(
      "person-id" -> Value(person.id),
      "person-name" -> Value(person.fullname)
    )}
  )

  def view(person: Person) = Transform (
    "person-id" -> Value(person.id),
    "person-firstname" -> Value(person.firstname),
    "person-lastname" -> Value(person.lastname),
    "person-fullname" -> Value(person.fullname)
  )

  object Firstname extends Params.Extract(
    "firstname", Params.first ~> Params.nonempty
  )

  object Lastname extends Params.Extract(
    "lastname", Params.first ~> Params.nonempty
  )

  def intent = {
    case GET(Path("/")) =>
      val file = new File(base, "index.lmxml")
      val format = list andThen XmlConvert
      val response = Lmxml.fromFile(file)(format)

      Status(200) ~>
      ContentType("text/html") ~>
      ResponseString(response.toString)

    case GET(Path("/new")) =>
      val file = new File(base, "new.lmxml")
      val response = Lmxml.fromFile(file)(XmlConvert)
      
      Status(200) ~>
      ContentType("text/html") ~>
      ResponseString(response.toString)

    case GET(Path(Seg("view" :: id :: Nil))) =>
      val person = Database.get(id.toInt).get

      val file = new File(base, "view.lmxml")
      val format = view(person) andThen XmlConvert
      val response = Lmxml.fromFile(file)(format)

      Status(200) ~>
      ContentType("text/html") ~>
      ResponseString(response.toString)

    case GET(Path(Seg("delete" :: id :: Nil))) =>
      Database.remove(id.toInt)      
      Redirect("/")

    case POST(Path("/save")) & Params(Firstname(fn) & Lastname(ln))=>
      val person = Person(fn, ln)

      Database.save(person)

      Redirect("/")
  }
}

object Examples {
  def main(args: Array[String]) {
    Http(8080).context("/css") { _.filter(Resources) }.filter(ContactsApp).run()
    Lmxml.storage.clear()
  }
}
