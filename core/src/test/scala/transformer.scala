package lmxml
package test

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
          div .post
            div .post-title posts-title
            div .post-body posts-body
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
                LmxmlNode("div", Map("class" -> "post"), List(
                  LmxmlNode("div", Map("class" -> "post-title"), List(
                    TextNode("1: Test")
                  )),
                  LmxmlNode("div", Map("class" -> "post-body"), List(
                    TextNode("What it is")
                  ))
                )),
                LmxmlNode("div", Map("class" -> "post"), List(
                  LmxmlNode("div", Map("class" -> "post-title"), List(
                    TextNode("2: Yo")
                  )),
                  LmxmlNode("div", Map("class" -> "post-body"), List(
                    TextNode("Tell me more")
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
      "posts" -> Foreach(posts.zipWithIndex,
        "posts-title" -> Fill[(Post, Int)](p =>"%d: %s" format(p._2 + 1, p._1.title)),
        "posts-body" -> Fill[(Post, Int)](_._1.body)
      )
    )

    val result = Lmxml.convert(contents)(transform)

    result should be === expected
  }
}
