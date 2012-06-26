package lmxml
package test

import transforms._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TransformSpec extends FlatSpec with ShouldMatchers {

  case class Post(id: Int, title: String, body: String)

  val outerTest: Option[String] = Some("Stuff")

  val innerTest: Option[String] = None

  val contents = """
html
  head title "Blog"
  body
    div #page
      div #header h1 "Bloggers"
      div .contents
        posts
          if-check
            p "Another way to check"
          else
            if-other-check
              p "deeper if that doesn't work"
          post-check
            post-check-true
              div .post
                div .post-title
                  a @href="/view/{ post-id }" post-title
                div .post-body post-body
            post-check-false
              div .other
                div .error "Weird, huh?"
"""

  "Tranform" should "inject data into a parsed lmxml nodes" in {

    val expected = List(
      LmxmlNode("html", children = List(
        LmxmlNode("head", children = List(
          LmxmlNode("title", children = List(
            TextNode("Blog")
          ))
        )),
        LmxmlNode("body", children = List(
          LmxmlNode("div", Map("id" -> "page"), List(
            LmxmlNode("div", Map("id" -> "header"), List(
              LmxmlNode("h1", children = List(TextNode("Bloggers")))
            )),
            LmxmlNode("div", Map("class" -> "contents"), List(
              TextNode("", children = List(
                TextNode("", children = List(
                  LmxmlNode("p", children = List(
                    TextNode("Another way to check")
                  ))
                )),
                TextNode("", children = List(
                  TextNode("", children = List(
                    LmxmlNode("div", Map("class" -> "post"), List(
                      LmxmlNode("div", Map("class" -> "post-title"), List(
                        LmxmlNode("a", Map("href" -> "/view/1"), List(
                          TextNode("Test")
                        ))
                      )),
                      LmxmlNode("div", Map("class" -> "post-body"), List(
                        TextNode("What it is")
                      ))
                    ))
                  )),
                  TextNode("")
                )),
                TextNode("", children = List(TextNode(""))),
                TextNode("", children = List(
                  TextNode(""),
                  TextNode("", children = List(
                    LmxmlNode("div", Map("class" -> "other"), List(
                      LmxmlNode("div", Map("class" -> "error"), List(
                        TextNode("Weird, huh?")
                      ))
                    ))
                  ))
                ))
              ))
            ))
          ))
        ))
      ))
    )

    val posts = List(Post(1, "Test", "What it is"), Post(2, "Yo", "Tell me more"))

    val transform = Transform(
      "posts" -> Foreach(posts) { post => Seq(
          "if-check" -> (If (post.id == 1)() orElse {
            Seq("if-other-check" -> If (post.id == 3) ())
          }),
          "post-check" -> (If (post.title.contains("Test")) {
            Seq(
              "post-id" -> Value(post.id),
              "post-title" -> Value(post.title),
              "post-body" -> Value(post.body)
            )
          } orElse {
            Seq(
              "post-error" -> Value(post.title + " - ERROR"),
              "post-test" -> If (outerTest.isEmpty) {
                Seq("explode" -> Value(innerTest.get))
              }
            )
          })
        )
      }
    )

    val result = DefaultLmxmlParser.fullParse(contents)(transform)

    result should be === expected
  }
}
