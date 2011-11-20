package lmxml
package test

import transforms._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TransformSpec extends FlatSpec with ShouldMatchers {

  case class Post(title: String, body: String)

  val contents = """
html
  head title "Blog"
  body
    div #page
      div #header h1 "Bloggers"
      div .contents
        posts
          post-check
            post-check-true
              div .post
                div .post-title post-title
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
                  TextNode("", children = List(
                    LmxmlNode("div", Map("class" -> "post"), List(
                      LmxmlNode("div", Map("class" -> "post-title"), List(
                        TextNode("Test")
                      )),
                      LmxmlNode("div", Map("class" -> "post-body"), List(
                        TextNode("What it is")
                      ))
                    ))
                  )),
                  TextNode("")
                )),
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

    val posts = List(Post("Test", "What it is"), Post("Yo", "Tell me more"))

    val transform = Transform(
      "posts" -> Foreach(posts) { post => Seq(
          "post-check" -> If(post.title.contains("Test")) { 
            Seq(
              "post-title" -> Value(post.title),
              "post-body" -> Value(post.body)
            ) 
          }.orElse {
            Seq("post-error" -> Value(post.title + " - ERROR"))
          }
        )
      }
    )

    val result = Lmxml.convert(contents)(transform)

    result should be === expected
  }
}
