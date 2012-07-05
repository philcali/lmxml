package lmxml
package markdown

import com.tristanhunt.knockoff.{
  Discounter,
  Block
}

trait MarkdownParsing extends LmxmlParsers with Discounter {
  def markdownNode: Parser[TopLevel] =
    "md" ~> (stringLit | multiLine | strWrapper) ^^  {
      s => TextNode(toXHTML(knockoff(s)).toString, true, _)
    }

  override def topLevel = markdownNode | super.topLevel
}
